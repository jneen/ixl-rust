use std::io::Read;

/**
 * The AST
 */
pub enum Term {
	Block(Vec<Command>),
	Subst(Vec<Command>),
	Variable(String),
	NumberLiteral(u32),
	StringLiteral(String),
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

pub struct Program(Vec<Command>);

/**
 * The Scanner
 */
pub struct Scanner {
	data: Vec<char>,
	index: usize,
	line: usize,
	col: usize
}

impl Scanner {
	
	fn new() -> Scanner {
		Scanner {
			data: Vec::new(),
			index: 0,
			line: 0,
			col: 0
		}
	}
	
	fn from_reader<T: Read>(reader: &mut T) -> Scanner {
		let mut buf = String::new();
		reader.read_to_string(&mut buf);
		Scanner::with_data(buf)
	}
	
	fn with_data(data: String) -> Scanner {
		let mut scanner = Scanner::new();
		scanner.data = data.chars().collect();
		scanner
	}
	
	fn eof(&self) -> bool { self.get_ch().is_none() }
	
	fn get_ch(&self) -> Option<char> {
		if self.index >= self.data.len() { None }
		else { Some(self.data[self.index]) }
	}
	
	fn peek(&self) -> Option<char> {
		if self.index + 1 >= self.data.len() { None }
		else { Some(self.data[self.index + 1]) }
	}

	fn bump(&mut self) {
		
		if let Some(ch) = self.get_ch() {
			if ch == '\n' {
				self.line += 1;
				self.col = 0;
			}
			else { self.col += 1; }
		}
		
		self.index += 1;
		
		// println!("bump! cursor: [{}]", self.get_ch);
	}

	fn consume<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
		let mut result = String::new();
		while let Some(ch) = self.get_ch() {
			if !pred(ch) { break }
			result.push(ch);
			self.bump();
		}
		result
	}

	fn consume_escaped<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
		let mut result = String::new();
		while let Some(ch) = self.get_ch() {
			if !pred(ch) { break }
			if ch == '\\' {
				self.bump();
				if self.eof() { self.error("unterminated escape sequence") }
			}

			result.push(ch);
			self.bump();
		}
		result
	}
	
	fn error(&self, msg: &str) -> ! {
		panic!("ixl: parse error at line {}:{}: {}", self.line + 1, self.col + 1, msg);
	}

	fn parse_spaces(&mut self) {
		self.consume(is_space);

		while self.get_ch() == Some('\\') && self.peek() == Some('\n') {
			self.bump();
			self.bump();
			self.consume(is_space);
		}
	}
	
	fn parse_block(&mut self) -> Term {
		if self.get_ch() != Some('[') { self.error("expected a block"); }
		self.bump();
		Term::Block(self.parse_commands_until(']'))
	}
	
	fn parse_subst(&mut self) -> Term {
		if self.get_ch() != Some('(') { self.error("expected a block"); }
		self.bump();
		Term::Subst(self.parse_commands_until(')'))
	}
	
	fn parse_commands_until(&mut self, end: char) -> Vec<Command> {
		let mut result: Vec<Command> = Vec::new();
		while !self.eof() {
			self.parse_termspaces();
			if self.get_ch() == Some(end) {
				self.bump();
				break;
			}
			result.push(self.parse_command());
		}
		result
	}
	
	fn parse_termspaces(&mut self) {
		self.consume(is_termspace);

		while self.get_ch() == Some('#') {
			self.consume(|x| x != '\n');
			self.consume(is_termspace);
		}
	}

	fn parse_string(&mut self) -> String {
		if self.get_ch() != Some('{') {
			return self.consume(|x| !is_word_terminator(x));
		}
		
		let mut result = String::new();
		self.braces(&mut result);
		result
	}

	fn braces(&mut self, out: &mut String) {
		let mut brace_count: usize = 1;

		loop {
			self.bump();
			match self.get_ch() {
				Some('{') => {
					out.push('{');
					brace_count += 1;
				},
				Some('}') => {
					brace_count -= 1;
					if brace_count == 0 { break; }
					out.push('}');
				},
				Some('\\') => {
					if self.eof() { self.error("unterminated braces"); }
					self.bump();
					out.push('\\');
				},
				Some(c) => out.push(c),
				None => self.error("unterminated braces")
			}
		}

		self.bump();
	}

	fn bareword<F: Fn(char)>(&mut self, callback: F) {
		while let Some(ch) = self.get_ch() {
			if is_word_terminator(ch) { break }
			callback(ch);
			self.bump();
		}
	}

	fn parse_varname(&mut self) -> String {
		if self.get_ch() == Some('{') {
			let mut result = String::new();
			self.braces(&mut result);
			result
		}
		else { self.consume(|c| char::is_alphanumeric(c) || "-_".contains(c)) }
	}

	fn parse_bareword(&mut self) -> Vec<Term> {
		let mut result: Vec<Term> = Vec::new();
		while let Some(ch) = self.get_ch() {
			if is_word_terminator(ch) { break }
			result.push(
				if ch == '$' { self.parse_interp_dollar() }
				else { Term::StringLiteral(self.consume_escaped(|s| s != '$' && !is_word_terminator(s))) }
			);
		}
		result
	}

	fn parse_interp_dollar(&mut self) -> Term {
		self.bump(); // skip the dollar
		
		// $(subst command)
		if self.get_ch() == Some('(') { self.parse_subst() }
		// ${var} and $var
		else { Term::Variable(self.parse_varname()) }
	}

	// TODO
	fn parse_interp_string(&mut self) -> Vec<Term> {
		if self.get_ch() != Some('{') { return self.parse_bareword(); }
		self.bump(); // consume initial open brace

		let mut brace_count: usize = 1;

		// TODO: dedup this code with self.braces()
		let mut result: Vec<Term> = Vec::new();
		while brace_count != 0 {
			match self.get_ch() {
				Some('$') => result.push(self.parse_interp_dollar()),
				Some(_) => {
					// scan the next string segment
					let mut string_component = String::new();
					
					loop {
						self.bump();
						match self.get_ch() {
							Some('{') => {
								brace_count += 1;
								string_component.push('{');
							},
							Some('}') => {
								brace_count -= 1;
								if brace_count == 0 { break }
								string_component.push('}');
							},
							Some('$') => break,
							Some(c) => string_component.push(c),
							None => self.error("unterminated braces")
						}
					}
	
					if &string_component != "" {
						result.push(Term::StringLiteral(string_component));
					}
				},
				None => self.error("unterminated braces")
			}
		}
		result
	}

	fn parse_term(&mut self) -> Term {
		match self.get_ch() {
			Some('$') => {
				self.bump();
				Term::Variable(self.parse_varname())
			},
			Some('[') => self.parse_block(),
			Some('(') => self.parse_subst(),
			Some('\'') => {
				self.bump();
				Term::StringLiteral(self.parse_string())
			},
			Some('"') => {
				self.bump();
				Term::Interp(self.parse_interp_string())
			},
			Some(_) => Term::Interp(self.parse_bareword()),
			None => self.error("expected term, got eof")
		}
	}

	fn parse_command(&mut self) -> Command {
		let target = if self.get_ch() == Some('@') {
			self.bump();
			Some(self.parse_term())
		}
		else { None };

		if self.eof() { self.error("expected command, got eof") }

		self.parse_spaces();

		// look for flags
		let mut components: Vec<Component> = Vec::new();
		while let Some(ch) = self.get_ch() {
			if is_word_terminator(ch) { break }
			if ch == '-' {
				self.bump();
				if self.get_ch() == Some('-') { self.bump() }
				components.push(Component::Flag(self.parse_string()));
			}
			else { components.push(Component::Argument(self.parse_term())) }
			
			self.parse_spaces();
		}

		// pipes can be after comments or newlines,
		// but not semicolons.
		if self.get_ch() != Some(';') { self.parse_termspaces() }

		Command {
			target: target,
			
			components: components,
			
			pipe: if self.get_ch() == Some('|') { 
				self.bump();
				self.parse_spaces();
				Some(Box::new(self.parse_command()))
			}
			else { None }
		}
	}

	fn parse(&mut self) -> Program {
		let mut commands: Vec<Command> = Vec::new();
		while !self.eof() {
			self.parse_termspaces();
			commands.push(self.parse_command());
		}
		
		Program(commands)
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

fn with_scanner<F: Fn(Scanner) -> T>(s: &str, lambda: F) -> T {
	lambda(Scanner::with_data(s.to_string()))
}

#[test]
fn test_scanner() {
	with_scanner("hello world", |scanner| {
		let result = scanner.consume(char::is_alphanumeric);
		assert!(result == "hello");
	})
}

#[test]
fn test_strings() {
	with_scanner("{he{ll}o}\n{a\\{b}", |scanner| {
		let mut result = scanner.parse_string();
		assert!(result == "he{ll}o");

		scanner.parse_termspaces();

		result = scanner.parse_string();
		assert!(result == "a{b");
	})
}

#[test]
fn test_terms() {
	with_scanner("$foo 'bar $", |scanner| {
		let mut result = scanner.parse_term();
		assert!(if let Term::Variable(x) = result { x == "foo" } else { false });

		scanner.parse_termspaces();

		result = scanner.parse_term();
		assert!(if let Term::StringLiteral(x) = result { x == "bar" } else { false });
	})
}

#[test]
fn test_dots() {
	with_scanner("$ $", |scanner| {
		let mut result = scanner.parse_term();
		assert!(if let Term::Variable(x) = result { x.is_empty() } else { false });

		scanner.parse_termspaces();

		result = scanner.parse_term();
		assert!(if let Term::Variable(x) = result { x.is_empty() } else { false });
	})
}

#[test]
fn test_command() {
	let c1 = with_scanner("foo -a", |s| s.parse_command());
	assert!(c1.target.is_none());
	assert!(c1.components.len() == 2);
	assert_eq!(c1.components[0], Component::Argument(Term::Interp(vec![Term::StringLiteral("foo".to_string())])));
	assert_eq!(c1.components[1], Component::Flag("a".to_string()));

	let c2 = with_scanner("@'foo 'bar --why '1 $baz", |s| s.parse_command());
	assert_eq!(c2.target, Some(Term::StringLiteral("foo".to_string())));
	assert!(c2.components.len() == 4);
	assert_eq!(c2.components[0], Component::Argument(Term::StringLiteral("bar".to_string())));
	assert_eq!(c2.components[1], Component::Flag("why".to_string()));
	assert_eq!(c2.components[2], Component::Argument(Term::StringLiteral("1".to_string())));
	assert_eq!(c2.components[3], Component::Argument(Term::Variable("baz".to_string())));

	let c3 = with_scanner("'foo | 'bar", |s| s.parse_command());
	if let Some(ref bar) = c3.pipe {
		assert!(bar.components.len() == 1);
		assert_eq!(bar.components[0], Component::Argument(Term::StringLiteral("bar".to_string())));
	}
	else { panic!() }
}

#[test]
fn test_block() {
	if let Term::Block(ref commands) = with_scanner("[$ $]", |s| s.parse_block()) {
		assert!(commands.len() == 1);
		assert!(commands[0].components.len() == 2);
		assert_eq!(commands[0].components[0], Component::Argument(Term::Variable("".to_string())));
		assert_eq!(commands[0].components[1], Component::Argument(Term::Variable("".to_string())));
	}
	else { panic!() }
}

#[test]
fn test_interp() {
	let i1 = with_scanner("foo/$.txt", |s| s.parse_term());
	assert_eq!(i1,
		Term::Interp(vec![
			Term::StringLiteral("foo/".to_string()),
			Term::Variable("".to_string()),
			Term::StringLiteral(".txt".to_string())
		])
	);

	let i2 = with_scanner("foo/$baz", |s| s.parse_term());
	assert_eq!(i2,
		Term::Interp(vec![
			Term::StringLiteral("foo/".to_string()),
			Term::Variable("baz".to_string())
		])
	);

	let i2 = with_scanner("\\$100", |s| s.parse_term());
	assert_eq!(i2,
		Term::Interp(vec![Term::StringLiteral("$100".to_string())])
	);

	let i3 = with_scanner("foo/${}baz", |s| s.parse_term());
	assert_eq!(i3,
		Term::Interp(vec![
			Term::StringLiteral("foo/"),
			Term::Variable(""),
			Term::StringLiteral("baz")
		])
	);

	let i4 = with_scanner("foo/$(baz zot)", |s| s.parse_term());
	assert_eq!(i4,
		Term::Interp(vec![
			Term::StringLiteral("foo/".to_string()),
			Term::Subst(vec![
				Command {
					target: None,
					pipe: None,
					components: vec![
						Component::Argument(Term::Interp(vec![Term::StringLiteral("baz".to_string())])),
						Component::Argument(Term::Interp(vec![Term::StringLiteral("zot".to_string())]))
					]
				}
			])
		])
	);

	let i5 = with_scanner("\"{foo $bar baz}", |s| s.parse_term());
	assert_eq!(i5,
		Term::Interp(vec![
			Term::StringLiteral("foo ".to_string()),
			Term::Variable("bar".to_string()),
			Term::StringLiteral(" baz".to_string())
		])
	);

	let i6 = with_scanner("\"{foo {}$(baz zot)}", |s| s.parse_term());
	assert_eq!(i6,
		Term::Interp(vec![
			Term::StringLiteral("foo {}".to_string()),
			Term::Subst(vec![
				Command {
					target: None,
					pipe: None,
					components: [
						Component::Argument(Term::Interp(vec![Term::StringLiteral("baz".to_string())])),
						Component::Argument(Term::Interp(vec![Term::StringLiteral("zot".to_string())]))
					]
				}
			])
		])
	);
}
*/
