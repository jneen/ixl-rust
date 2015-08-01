use std::io::Read;

/**
 * The AST
 */
pub enum Term {
	Block(Vec<Command>),
	Subst(Vec<Command>),
	Variable(String),
	String(String),
	Interp(Vec<Term>)
}

pub enum Component {
	Flag(String),
	Argument(Term)
}

pub struct Command {
	target: Option<Term>,
	components: Vec<Component>,
	pipe: Option<Box<Command>>
}

pub struct Program {
	commands: Vec<Command>
}

/**
 * The Scanner
 */
pub struct Scanner {
	data: Vec<char>,
	index: usize,
	line: usize,
	col: usize
}

pub fn Scanner<T: Read>(reader: &mut T) -> Scanner {
	let mut buf = String::new();
	reader.read_to_string(&mut buf);
	
	assert!(buf.len() != 0, "No data received.");
	
	Scanner {
		data: buf.chars().collect(),
		index: 0,
		line: 0,
		col: 0
	}
}

impl Scanner {
	
	fn eof(&self) -> bool { self.curr_char().is_none() }
	
	fn curr_char(&self) -> Option<char> {
		if self.index >= self.data.len() { None }
		else { Some(self.data[self.index]) }
	}
	
	fn peek(&self) -> Option<char> {
		if self.index + 1 >= self.data.len() { None }
		else { Some(self.data[self.index + 1]) }
	}

	fn bump(&mut self) {
		
		match self.curr_char() {
			Some('\n') => {
				self.line += 1;
				self.col = 0;
			},
			Some(_) => self.col += 1,
			None => (),
		}
		
		self.index += 1;
		
		// println!("bump! cursor: [{}]", self.curr_char);
	}

	fn consume<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
		let mut result = String::new();
		while !self.eof() && pred(self.curr_char().unwrap()) {
			result.push(self.curr_char().unwrap());
			self.bump();
		}
		result
	}

	fn consume_escaped<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
		let mut result = String::new();
		while !self.eof() && pred(self.curr_char().unwrap()) {
			if self.curr_char() == Some('\\') {
				self.bump();
				if self.eof() { self.error("unterminated escape sequence"); }
			}

			result.push(self.curr_char().unwrap());
			self.bump();
		}
		result
	}
	
	fn error(&self, msg: &str) -> ! {
		panic!("ixl: parse error at line {}:{}: {}", self.line + 1, self.col + 1, msg);
	}

	fn parse_spaces(&mut self) {
		self.consume(is_space);

		while self.curr_char() == Some('\\') && self.peek() == Some('\n') {
			self.bump();
			self.bump();
			self.consume(is_space);
		}
	}
	
	fn parse_block(&mut self) -> Term {
		if self.curr_char() != Some('[') { self.error("expected a block"); }
		self.bump();
		Term::Block(self.parse_commands_until(']'))
	}
	
	fn parse_subst(&mut self) -> Term {
		if self.curr_char() != Some('(') { self.error("expected a block"); }
		self.bump();
		Term::Subst(self.parse_commands_until(')'))
	}
	
	fn parse_commands_until(&mut self, end: char) -> Vec<Command> {
		let mut result: Vec<Command> = Vec::new();
		while !self.eof() {
			self.parse_termspaces();
			if self.curr_char() == Some(end) {
				self.bump();
				break;
			}
			result.push(self.parse_command());
		}
		result
	}
	
	fn parse_termspaces(&mut self) {
		self.consume(is_termspace);

		while self.curr_char() == Some('#') {
			self.consume(|x| x != '\n');
			self.consume(is_termspace);
		}
	}

	fn parse_string(&mut self) -> String {
		if self.curr_char() != Some('{') {
			return self.consume(|x| !is_word_terminator(x));
		}
		
		// XXX may cause problems
		let mut result = String::new();
		self.braces(&mut result);
		result
	}

	fn braces(&mut self, out: &mut String) {
		self.bump(); // consume initial open brace
		if self.eof() { self.error("unterminated braces"); }

		let mut brace_count: usize = 1;

		loop {
			match self.curr_char().unwrap() {
				'{' => {
					out.push('{');
					brace_count += 1;
				},
				'}' => {
					brace_count -= 1;
					if brace_count == 0 { break; }
					out.push('}');
				},
				'\\' => {
					if self.eof() { self.error("unterminated braces"); }
					self.bump();
					out.push('\\');
				},
				c @ _ => out.push(c)
			}

			self.bump();
			if self.eof() { self.error("unterminated braces"); }
		}

		if self.eof() { self.error("unterminated braces"); }
		self.bump();
	}

	fn bareword<F: Fn(char)>(&mut self, callback: F) {
		while !self.eof() && !is_word_terminator(self.curr_char().unwrap()) {
			callback(self.curr_char().unwrap());
			self.bump()
		}
	}

	fn parse_varname(&mut self) -> String {
		match self.curr_char() {
			Some('{') => {
				// XXX: may cause problems
				let mut result = String::new();
				self.braces(&mut result);
				result
			},
			_ => self.consume(|c| char::is_alphanumeric(c) || "-_".contains(c))
		}
	}

	fn parse_bareword(&mut self) -> Vec<Term> {
		let mut result: Vec<Term> = Vec::new();
		while !self.eof() && !is_word_terminator(self.curr_char().unwrap()) {
			result.push(match self.curr_char().unwrap() {
				'$' => self.parse_interp_dollar(),
				_ => Term::String(self.consume_escaped(|s| s != '$' && !is_word_terminator(s)))
			})
		}
		result
	}

	fn parse_interp_dollar(&mut self) -> Term {
		self.bump(); // skip the dollar

		match self.curr_char() {
			// $(subst command)
			Some('(') => self.parse_subst(),
			// ${var} and $var
			_ => Term::Variable(self.parse_varname())
		}
	}

	// TODO
	fn parse_interp_string(&mut self) -> Vec<Term> {
		if self.curr_char() != Some('{') { return self.parse_bareword(); }
		self.bump(); // consume initial open brace
		if self.eof() { self.error("unterminated braces"); }

		let mut brace_count: usize = 1;

		// TODO: dedup this code with self.braces()
		let mut result: Vec<Term> = Vec::new();
		loop {
			if brace_count == 0 { break; }
			match self.curr_char().unwrap() {
				'$' => result.push(self.parse_interp_dollar()),
				_ => {
					// scan the next string segment
					let mut string_component = String::new();
					
					loop {
						if self.eof() { self.error("unterminated braces"); }
						match self.curr_char().unwrap() {
							'{' => { 
								brace_count += 1;
								string_component.push('{');
							},
							'}' => {
								brace_count -= 1;
								if brace_count == 0 { break; }
								string_component.push('}');
							},
							'$' => { break; },
							c @ _ => string_component.push(c)
						}
						self.bump();
					}

					if &string_component != "" {
						result.push(Term::String(string_component));
					}
				}
			}
		}
		result
	}

	fn parse_term(&mut self) -> Term {
		match self.curr_char().unwrap() {
			'$' => {
				self.bump();
				Term::Variable(self.parse_varname())
			},
			'[' => self.parse_block(),
			'(' => self.parse_subst(),
			'\'' => {
				self.bump();
				Term::String(self.parse_string())
			},
			'"' => {
				self.bump();
				Term::Interp(self.parse_interp_string())
			},
			_ => Term::Interp(self.parse_bareword())
		}
	}

	fn parse_command(&mut self) -> Command {
		let target = if self.curr_char() == Some('@') {
			self.bump();
			// possible eof not handled here?
			Some(self.parse_term())
		}
		else { None };

		if self.eof() { self.error("expected command, got eof"); }

		self.parse_spaces();

		// look for flags
		let mut components: Vec<Component> = Vec::new();
		while !self.eof() {
			if is_word_terminator(self.curr_char().unwrap()) { break; }

			match self.curr_char().unwrap() {
				'-' => {
					self.bump();
					if self.curr_char() == Some('-') { self.bump(); }
					components.push(Component::Flag(self.parse_string()));
				},
				_ => components.push(Component::Argument(self.parse_term()))
			}

			self.parse_spaces();
		}

		// pipes can be after comments or newlines,
		// but not semicolons.
		if self.curr_char() != Some(';') { self.parse_termspaces() }

		let pipe = if self.curr_char() == Some('|') { 
			self.bump();
			self.parse_spaces();
			Some(Box::new(self.parse_command()))
		}
		else { None };

		Command {
			target: target,
			components: components,
			pipe: pipe,
		}
	}

	fn parse(&mut self) -> Program {
		let mut commands: Vec<Command> = Vec::new();
		while !self.eof() {
			self.parse_termspaces();
			commands.push(self.parse_command());
		}
		
		Program { commands: commands }
	}
}

fn is_space(ch: char) -> bool {
	" \t".contains(ch)
}

fn is_termspace(ch: char) -> bool {
	is_space(ch) || "\n;\r".contains(ch)
}

fn is_word_terminator(ch: char) -> bool {
	is_termspace(ch) || "#])|".contains(ch)
}

/*

fn with_scanner<T>(s: &str, yield: fn(Scanner) -> T) -> T {
	io::with_str_reader(s, |r| yield(Scanner(r)))
}

#[test]
fn test_scanner() {
	do with_scanner("hello world") |scanner| {
		let result = scanner.consume(char::is_alphanumeric);
		assert(result) == ~"hello";
	}
}

#[test]
fn test_strings() {
	do with_scanner(~"{he{ll}o}\n{a\\{b}") |scanner| {
		let result1 = scanner.parse_string();
		assert(result1 == ~"he{ll}o");

		scanner.parse_termspaces();

		let result2 = scanner.parse_string();
		assert(result2 == ~"a{b");
	}
}

#[test]
fn test_terms() {
	do with_scanner(~"$foo 'bar $") |scanner| {
		let result1 = scanner.parse_term();
		assert(match result1 {
			Variable(x) => { x == ~"foo" }
			_ => { false }
		});

		scanner.parse_termspaces();

		let result2 = scanner.parse_term();
		assert(match result2 {
			String(x) => x == ~"bar", _ => false
		});
	}
}

#[test]
fn test_dots() {
	do with_scanner("$ $") |scanner| {
		let result1 = scanner.parse_term();
		assert(match result1 {
			Variable(x) => x == ~"", _ => false
		});

		scanner.parse_termspaces();

		let result2 = scanner.parse_term();
		assert(match result2 {
			Variable(x) => x == ~"", _ => false
		});
	}
}

macro_rules! matches (
	($e:expr, $p:pat => $cond:expr) => (
		match $e { $p => $cond, _ => false }
	);
	($e:expr, $p:pat) => (matches!($e, $p => true));
)

#[test]
fn test_command() {
	let c1 = with_scanner(~"foo -a", |s| s.parse_command());
	assert(match c1.target { None => true, _ => false });
	assert(c1.components.len() == 2);
	assert(matches!(*c1.components[0], Argument(Interp([String(~"foo")]))));
	assert(matches!(*c1.components[1], Flag(~"a")));

	let c2 = with_scanner(~"@'foo 'bar --why '1 $baz", |s| s.parse_command());
	assert(matches!(c2.target, Some(String(~"foo"))));
	assert(c2.components.len() == 4);
	assert(matches!(*c2.components[0], Argument(String(~"bar"))));
	assert(matches!(*c2.components[1], Flag(~"why")));
	assert(matches!(*c2.components[2], Argument(String(~"1"))));
	assert(matches!(*c2.components[3], Argument(Variable(~"baz"))));

	let c3 = with_scanner(~"'foo | 'bar", |s| s.parse_command());
	match c3.pipe {
		Some(ref bar) => {
			assert(bar.components.len() == 1);
			assert(matches!(*bar.components[0], Argument(String(~"bar"))));
		}
		_ => { fail }
	}
}

#[test]
fn test_block() {
	let b1 = with_scanner(~"[$ $]", |s| s.parse_block());
	match b1 {
		Block(ref commands) => {
			assert(commands.len() == 1);
			assert(commands[0].components.len() == 2);
			assert(
				matches!(*commands[0].components[0], Argument(Variable(~"")))
			);
			assert(
				matches!(*commands[0].components[1], Argument(Variable(~"")))
			);
		}
		_ => { fail; }
	}
}

#[test]
fn test_interp() {
	let i1 = with_scanner(~"foo/$.txt", |s| s.parse_term());
	assert(matches!(i1,
		Interp([String(~"foo/"), Variable(~""), String(~".txt")])
	));

	let i2 = with_scanner(~"foo/$baz", |s| s.parse_term());
	assert(matches!(i2,
		Interp([String(~"foo/"), Variable(~"baz")])
	));

	let i2 = with_scanner(~"\\$100", |s| s.parse_term());
	assert(matches!(i2,
		Interp([String(~"$100")])
	));

	let i3 = with_scanner(~"foo/${}baz", |s| s.parse_term());
	assert(matches!(i3,
		Interp([String(~"foo/"), Variable(~""), String(~"baz")])
	));

	let i4 = with_scanner(~"foo/$(baz zot)", |s| s.parse_term());
	assert(matches!(i4,
		Interp([String(~"foo/"), Subst([
			@Command { target: None, pipe: None, components: [
				@Argument(Interp([String(~"baz")])),
				@Argument(Interp([String(~"zot")]))
			]}
		])])
	));

	let i5 = with_scanner(~"\"{foo $bar baz}", |s| s.parse_term());
	assert(matches!(i5,
		Interp([String(~"foo "), Variable(~"bar"), String(~" baz")])
	));

	let i6 = with_scanner(~"\"{foo {}$(baz zot)}", |s| s.parse_term());
	assert(matches!(i6,
		Interp([String(~"foo {}"), Subst([
			@Command { target: None, pipe: None, components: [
				@Argument(Interp([String(~"baz")])),
				@Argument(Interp([String(~"zot")]))
			]}
		])])
	));
}
*/
