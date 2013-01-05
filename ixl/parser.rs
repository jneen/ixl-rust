extern mod std;

use io::{ReaderUtil,WriterUtil};
use either::{Left,Right,Either};

/**
 * The AST
 */
pub enum Term {
  Block(~[@Command]),
  Subst(~[@Command]),
  Variable(~str),
  String(~str),
}

pub enum Component {
  Flag(~str),
  Argument(@Term)
}

pub struct Command {
  target: Option<@Term>,
  components: ~[@Component],
  pipe: Option<@Command>
}

pub struct Program {
  commands: ~[@Command]
}

/**
 * The Scanner
 */
pub struct Scanner {
  reader: io::Reader,
  mut cursor: char,
  mut lookahead: Option<char>,
  mut line: uint,
  mut col: uint,
}

pub fn Scanner(reader: io::Reader) -> Scanner {
  let s = Scanner {
    reader: reader,
    cursor: 0 as char,
    lookahead: None,
    line: 1u, col: 0u,
  };

  s.bump();

  s
}

impl Scanner {
  fn eof(&self) -> bool { self.cursor == -1 as char }

  fn bump(&self) {
    assert(!self.eof());

    let mut lookahead = None;
    lookahead <-> self.lookahead;

    self.cursor = match lookahead {
      Some(ch) => { ch }
      None => { self.reader.read_char() }
    };

    if self.cursor == '\n' {
      self.line += 1u;
      self.col = 1u;
    }
    else {
      self.col += 1u;
    }

    // io::println(fmt!("bump! cursor: [%c]", self.cursor));
  }

  fn peek(&self) -> char {
    match self.lookahead {
      Some(ch) => { ch }
      None => {
        let ch = self.reader.read_char();
        self.lookahead = Some(ch);
        ch
      }
    }
  }

  fn consume(&self, pred: pure fn(char) -> bool) -> ~str {
    do io::with_str_writer |out| {
      while !self.eof() && pred(self.cursor) {
        out.write_char(self.cursor);
        self.bump();
      };
    }
  }

  fn error(msg: &str) -> ! {
    fail fmt!("ixl: parse error at line %u:%u: %s", self.line, self.col, msg);
  }

  fn parse_spaces(&self) {
    self.consume(is_space);

    while(self.cursor == '\\' && self.peek() == '\n') {
      self.bump(); self.bump();
      self.consume(is_space);
    }
  }

  fn parse_block(&self) -> Term {
    if self.cursor != '[' { self.error("expected a block"); }
    self.bump();
    Block(self.parse_commands_until(']'))
  }

  fn parse_subst(&self) -> Term {
    if self.cursor != '(' { self.error("expected a block"); }
    self.bump();
    Subst(self.parse_commands_until(')'))
  }

  fn parse_commands_until(&self, end: char) -> ~[@Command] {
    do vec::build |push| {
      while !self.eof() {
        self.parse_termspaces();
        if self.cursor == end {
          self.bump();
          break;
        }
        else {
          push(@self.parse_command());
        }
      }
    }
  }

  fn parse_termspaces(&self) {
    self.consume(is_termspace);

    while self.cursor == '#' {
      self.consume(|x| x != '\n');
      self.consume(is_termspace);
    }
  }

  fn parse_string(&self) -> ~str {
    if self.cursor != '{' {
      return self.consume(|x| !is_word_terminator(x));
    }

    self.bump();
    if self.eof() { self.error("unterminated string"); }
    let mut braceCount = 1u;

    do io::with_str_writer |out| {
      loop {
        match self.cursor {
          '{' => {
            out.write_char(self.cursor);
            braceCount += 1u;
          }
          '}' => {
            braceCount -= 1u;
            if braceCount == 0 { break; }
            else { out.write_char(self.cursor); }
          }
          '\\' => {
            if self.eof() { self.error("unterminated escape sequence"); }
            self.bump();
            out.write_char(self.cursor);
          }
          _ => { out.write_char(self.cursor); }
        }

        self.bump();
        if self.eof() { self.error("unterminated string"); }
      }

      if self.eof() { self.error("unterminated string"); }
      self.bump();
    }
  }

  fn parse_term(&self) -> Term {
    match self.cursor {
      '$' => {
        self.bump();
        Variable(self.parse_string())
      }
      '[' => {
        self.parse_block()
      }
      '(' => {
        self.parse_subst()
      }
      _ => { String(self.parse_string()) }
    }
  }

  fn parse_command(&self) -> Command {
    let target = if self.cursor == '@' {
      self.bump();
      Some(@self.parse_term())
    }
    else {
      None
    };

    if self.eof() { self.error("expected command, got eof"); }

    self.parse_spaces();

    // look for flags
    let components = do vec::build |push| {
      while !self.eof() {
        if is_word_terminator(self.cursor) { break; }

        match self.cursor {
          '-' => {
            self.bump();
            if self.cursor == '-' { self.bump(); }
            push(@Flag(self.parse_string())); }
          _ => { push(@Argument(@self.parse_term())); }
        }

        self.parse_spaces();
      }
    };

    // pipes can be after comments or newlines,
    // but not semicolons.
    if self.cursor != ';' { self.parse_termspaces() }

    let pipe = if self.cursor == '|' { 
      self.bump();
      self.parse_spaces();
      Some(@self.parse_command())
    }
    else { None };

    Command {
      target: target,
      components: components,
      pipe: pipe,
    }
  }

  fn parse(&self) -> Program {
    let commands = do vec::build |push| {
      while !self.eof() {
        self.parse_termspaces();
        push(@self.parse_command());
      }
    };

    Program { commands: commands }
  }
}

pure fn is_space(ch: char) -> bool {
  " \t".contains_char(ch)
}

pure fn is_termspace(ch: char) -> bool {
  is_space(ch) || "\n;\r".contains_char(ch)
}

pure fn is_word_terminator(ch: char) -> bool {
  is_termspace(ch) || "#])|".contains_char(ch)
}

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
  do with_scanner(~"$foo bar $") |scanner| {
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

#[test]
fn test_command() {
  let c1 = with_scanner(~"foo -a", |s| s.parse_command());
  assert(match c1.target { None => true, _ => false });
  assert(c1.components.len() == 2);
  assert(match *c1.components[0] {
    Argument(@String(ref x)) => *x == ~"foo",
    _ => false
  });

  assert(match *c1.components[1] { Flag(ref x) => *x == ~"a", _ => false });

  let c2 = with_scanner(~"@foo bar --why 1 $baz", |s| s.parse_command());
  assert(match c2.target { Some(@String(ref x)) => *x == ~"foo", _ => false });
  assert(c2.components.len() == 4);
  assert(match *c2.components[0] {
    Argument(@String(ref x)) => *x == ~"bar",
    _ => false
  });
  assert(match *c2.components[1] {
    Flag(ref x) => *x == ~"why",
    _ => false
  });
  assert(match *c2.components[2] {
    Argument(@String(ref x)) => *x == ~"1",
    _ => false
  });
  assert(match *c2.components[3] {
    Argument(@Variable(ref x)) => *x == ~"baz",
    _ => false
  });

  let c3 = with_scanner(~"foo | bar", |s| s.parse_command());
  match c3.pipe {
    Some(ref bar) => {
      assert(bar.components.len() == 1);
      assert(match *bar.components[0] {
        Argument(@String(ref x)) => *x == ~"bar",
        _ => false
      });
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
      assert(match *commands[0].components[0] {
        Argument(@Variable(ref x)) => *x == ~"", _ => false
      });
      assert(match *commands[0].components[1] {
        Argument(@Variable(ref x)) => *x == ~"", _ => false
      });
    }
    _ => { fail; }
  }
}
