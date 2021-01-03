#[derive(Clone, Debug, PartialEq)]
pub enum Category {
    Name(String),

    // Keywords
    And,
    Break,
    Do,
    Else,
    ElseIf,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,

    // Symbols
    Plus,
    Minus,
    Star,
    Slash,
    Slash2,
    Percent,
    Caret,
    Hash,
    Ampersand,
    Tilde,
    Pipe,
    LessThan2,
    GreaterThan2,
    Equal2,
    TildeEqual,
    LessThanEqual,
    GreaterThanEqual,
    LessThan,
    GreaterThan,
    Equal,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Colon2,
    SemiColon,
    Colon,
    Comma,
    Dot,
    Dot2,
    Dot3,

    // Literals
    Integer(i64),
    Float(f64),
    String(String),

    // Special
    Eof,
    Error(ScanError),
}

#[derive(Clone, Debug)]
pub struct Token {
    pub category: Category,
    line: u64,
    column: u64,
}

impl Default for Token {
    fn default() -> Token {
        Token {
            category: Category::Eof,
            line: 0,
            column: 0,
        }
    }
}

#[derive(Clone)]
pub struct Scanner<'a> {
    source: std::str::Chars<'a>,
    pub line: u64,
    column: u64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ScanError {
    InvalidEscape,
    UnexpectedCharacter(char),
    UnexpectedEof,
    MalformedNumber,
}

fn match_keyword(name: &str) -> Option<Category> {
    match name {
        "and" => Some(Category::And),
        "break" => Some(Category::Break),
        "do" => Some(Category::Do),
        "else" => Some(Category::Else),
        "elseif" => Some(Category::ElseIf),
        "end" => Some(Category::End),
        "false" => Some(Category::False),
        "for" => Some(Category::For),
        "function" => Some(Category::Function),
        "goto" => Some(Category::Goto),
        "if" => Some(Category::If),
        "in" => Some(Category::In),
        "local" => Some(Category::Local),
        "nil" => Some(Category::Nil),
        "not" => Some(Category::Not),
        "or" => Some(Category::Or),
        "repeat" => Some(Category::Repeat),
        "return" => Some(Category::Return),
        "then" => Some(Category::Then),
        "true" => Some(Category::True),
        "until" => Some(Category::Until),
        "while" => Some(Category::While),
        _ => None,
    }
}

fn match_name(scanner: &mut Scanner, prefix: char) -> Token {
    // latin alphanumeric chars or underscores, case-sensitive, non-keyword, doesn't start with digit
    let mut name = prefix.to_string();
    while let Some(c) = scanner.peek() {
        if c.is_ascii_alphanumeric() || c == '_' {
            if let Some(a) = scanner.advance() {
                name.push(a)
            }
        } else {
            break;
        }
    }
    scanner.emit(match_keyword(&name).unwrap_or(Category::Name(name)))
}

fn match_digits(scanner: &mut Scanner) -> String {
    let mut lexeme = String::new();
    while let Some(c) = scanner.peek() {
        if c.is_ascii_digit() {
            lexeme.push(c);
            scanner.advance();
        } else {
            break;
        }
    }
    lexeme
}

fn resume_exponent(scanner: &mut Scanner) -> Option<String> {
    let mut exp = String::new();
    if let Some(c) = scanner.peek() {
        if c == '+' || c == '-' {
            scanner.advance();
            exp.push(c);
        }
    }
    let exponent = match_digits(scanner);
    if exponent.is_empty() {
        None
    } else {
        exp += &exponent;
        Some(exp)
    }
}

fn resume_hex(scanner: &mut Scanner) -> Token {
    // assume 0x or 0X has been consumed
    let mut points = 0;
    let mut digits = 0;
    let mut lexeme = String::new();
    while let Some(c) = scanner.peek() {
        if c == '.' {
            lexeme.push(c);
            scanner.advance();
            points += 1;
        } else if c.is_ascii_hexdigit() {
            lexeme.push(c);
            scanner.advance();
            digits += 1;
        } else {
            break;
        }
    }
    if points > 1 || digits < 1 || lexeme.starts_with('.') {
        return scanner.emit(Category::Error(ScanError::MalformedNumber));
    }
    /*
    // TODO hex exponents
    if let Some(c) = scanner.peek() {
        if c == 'p' || c == 'P' {
            scanner.advance();
            lexeme.push(c);
            if let Some(exp) = resume_exponent(scanner) {
                lexeme += &exp;
                unimplemented!();
            } else {
                return Category::Error(ScanError::MalformedNumber);
            }
        }
    }
    */
    if points == 0 {
        // TODO if value overflows (e.g. 0xffffffffffffffff, it should
        // wrap around to fit into a valid integer)
        scanner.emit(Category::Integer(i64::from_str_radix(&lexeme, 16).unwrap()))
    } else {
        unimplemented!()
    }
}

fn match_numeral(scanner: &mut Scanner, prefix: char) -> Token {
    // has optional fraction and optional decimal exponent eE
    // also accepts hexadecimal constants (0x or 0X ...)
    // hex constants can have an optional fraction and exponent (pP ... instead of eE)
    // numbers with a radix point or an exponent are floats
    // otherwise, integer (if it fits, otherwise it's a float)
    let mut points = 0;
    let mut digits = 0;
    let mut lexeme = if prefix == '.' {
        points += 1;
        String::from("0.")
    } else {
        assert!(prefix.is_ascii_digit());
        digits += 1;
        prefix.to_string()
    };
    if lexeme == "0" {
        match scanner.peek() {
            Some('x') => {
                scanner.advance();
                return resume_hex(scanner);
            }
            Some('X') => {
                scanner.advance();
                return resume_hex(scanner);
            }
            _ => (),
        }
    }
    while let Some(c) = scanner.peek() {
        if c == '.' {
            lexeme.push('.');
            scanner.advance();
            points += 1;
        } else if c.is_ascii_digit() {
            lexeme.push(c);
            scanner.advance();
            digits += 1;
        } else {
            break;
        }
    }
    if points > 1 || digits < 1 {
        return scanner.emit(Category::Error(ScanError::MalformedNumber));
    }
    if let Some(c) = scanner.peek() {
        if c == 'e' || c == 'E' {
            scanner.advance();
            lexeme.push(c);
            if let Some(exp) = resume_exponent(scanner) {
                lexeme += &exp;
                return scanner.emit(Category::Float(lexeme.parse().unwrap()));
            } else {
                return scanner.emit(Category::Error(ScanError::MalformedNumber));
            }
        }
    }
    if points == 0 {
        scanner.emit(Category::Integer(lexeme.parse().unwrap()))
    } else {
        scanner.emit(Category::Float(lexeme.parse().unwrap()))
    }
}

fn escaped(c: char) -> Option<char> {
    // excludes: z (newline)
    match c {
        'a' => Some('\x07'), // alert/bell
        'b' => Some('\x08'), // backspace
        'f' => Some('\x0c'), // form feed
        'n' => Some('\n'),
        'r' => Some('\r'),
        't' => Some('\t'),
        'v' => Some('\x0b'), // vertical tab
        '\\' => Some('\''),
        '\'' => Some('\''),
        '"' => Some('"'),
        _ => None,
    }
}

fn match_string(scanner: &mut Scanner, delim: char) -> Token {
    // assume opening delim has been consumed already
    // short literal strings delimited by single or double quotes
    // can contain '\a' (bell), '\b' (backspace), '\f' (form feed),
    // '\\' (backslash), '\"' (quotation mark if delimited by double quotes),
    // '\'' (apostrophe if delimited by single quotes)
    // backslash followed by line break => newline in the string
    // '\z' skips the following span of whitespace chars (including line breaks)
    // invalid escape sequences and unescaped line breaks not allowed
    // `\xXX` (XX two hexdigits) or `\ddd` (3 decimal digits) can describe any byte
    // `\u{XXX}` unicode code points, but XXX < 2^31
    let mut string = String::new();
    let mut has_invalid_escape = false;
    loop {
        let c = if let Some(c) = scanner.peek() {
            c
        } else {
            return scanner.emit(Category::Error(ScanError::UnexpectedEof));
        };
        if c == delim {
            scanner.advance();
            return if has_invalid_escape {
                scanner.emit(Category::Error(ScanError::InvalidEscape))
            } else {
                scanner.emit(Category::String(string))
            };
        } else if c != '\\' {
            scanner.advance();
            string.push(c);
        } else {
            scanner.advance();
            match scanner.advance() {
                // TODO unicode, hex and digits
                Some('z') => skip_whitespace(scanner),
                None => return scanner.emit(Category::Error(ScanError::UnexpectedEof)),
                Some(e) => {
                    if let Some(a) = escaped(e) {
                        string.push(a);
                    } else {
                        has_invalid_escape = true;
                    }
                }
            }
        }
    }
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut scanner = Scanner {
            source: source.chars(),
            line: 1,
            column: 1,
        };
        if scanner.source.as_str().starts_with("#!") {
            skip_line(&mut scanner);
        }
        scanner
    }

    fn emit(&mut self, category: Category) -> Token {
        Token {
            category,
            line: self.line,
            column: self.column,
        }
    }

    /// Advance one character in the source code.
    /// The following characters are converted into a single newline.
    ///
    /// - Carriage return
    /// - Carriage return followed by a newline
    /// - Newline followed by a carriage return
    fn advance(&mut self) -> Option<char> {
        self.column += 1;
        match self.source.next() {
            Some('\n') => {
                self.line += 1;
                if let Some('\r') = self.peek() {
                    self.source.next();
                }
                Some('\n')
            }
            Some('\r') => {
                self.line += 1;
                if let Some('\n') = self.peek() {
                    self.source.next();
                }
                Some('\n')
            }
            default => default,
        }
    }

    fn advance_if_eq_else(&mut self, c: char, a: Category, b: Category) -> Token {
        match self.peek() {
            None => self.emit(b),
            Some(p) => {
                if p == c {
                    self.advance();
                    self.emit(a)
                } else {
                    self.emit(b)
                }
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.clone().next()
    }
}

fn is_whitespace(c: char) -> bool {
    match c {
        ' ' => true,
        '\n' => true,
        '\r' => true,
        '\t' => true,
        '\x0b' => true, // vertical tab
        '\x0c' => true, // form feed
        _ => false,
    }
}

fn skip_whitespace(scanner: &mut Scanner) {
    while let Some(lookahead) = scanner.peek() {
        if is_whitespace(lookahead) {
            scanner.advance();
        }
    }
}

fn resume_opening_long_bracket(scanner: &mut Scanner) -> Option<usize> {
    let checkpoint = scanner.source.clone();
    let mut level = 0;
    loop {
        match scanner.advance() {
            Some('=') => level += 1,
            Some('[') => return Some(level),
            _ => {
                scanner.source = checkpoint;
                return None;
            }
        }
    }
}

/// Tries to match opening long bracket (`[[`, `[=[`, `[==[`, ...) and
/// returns the bracket level (number of `=`s).
/// If it fails to match an opening bracket, then it reverts the scanner
/// to its state before the attempt and returns [`None`].
fn opening_long_bracket(scanner: &mut Scanner) -> Option<usize> {
    let checkpoint = scanner.source.clone();
    if let Some('[') = scanner.advance() {
    } else {
        scanner.source = checkpoint;
        return None;
    }
    resume_opening_long_bracket(scanner)
}

/// Assumes first `]` has been consumed, so it only checks for `=`s and
/// `]`.
/// Returns true if it successfully matches a clonsing long bracket.
/// Otherwise, it returns false and reverts the scanner to its state
/// before the attempt.
fn resume_closing_long_bracket(scanner: &mut Scanner, level: usize) -> bool {
    let checkpoint = scanner.source.clone();
    let mut count = 0;
    loop {
        match scanner.advance() {
            Some('=') => count += 1,
            Some(']') => {
                if count == level {
                    return true;
                } else {
                    scanner.source = checkpoint;
                    return false;
                }
            }
            _ => {
                scanner.source = checkpoint;
                return false;
            }
        }
    }
}

fn match_long_comment(scanner: &mut Scanner) {
    // assume -- has been consumed
    let level = if let Some(level) = opening_long_bracket(scanner) {
        level
    } else {
        return skip_line(scanner);
    };
    while let Some(c) = scanner.advance() {
        if c == ']' && resume_closing_long_bracket(scanner, level) {
            break;
        }
    }
}

// long literal strings delimited by [=[ (any number of '='s)
// and closed by ]=] with the same number of '='s as opening delim
// can have multiple lines, doesn't interpret escape sequences,
// carriage return, newline, carriage return followed by a newline,
// newline followed by a carriage return, etc. are all converted
// to a simple newline
// if the opening bracket is followed immediately by a newline,
// the newline is excluded from the string
fn resume_long_literal_string(scanner: &mut Scanner) -> Option<Category> {
    let checkpoint = scanner.source.clone();
    let level = if let Some(level) = resume_opening_long_bracket(scanner) {
        level
    } else {
        scanner.source = checkpoint;
        return None;
    };

    // Drop newline immediately after opening long bracket.
    match scanner.peek() {
        Some('\n') => {
            scanner.advance();
            if let Some('\r') = scanner.peek() {
                scanner.advance();
            }
        }
        Some('\r') => {
            scanner.advance();
            if let Some('\n') = scanner.peek() {
                scanner.advance();
            }
        }
        _ => (),
    }

    let mut val = String::new();
    loop {
        match scanner.advance() {
            Some('\n') => val.push('\n'),
            Some(']') => {
                if resume_closing_long_bracket(scanner, level) {
                    break;
                } else {
                    val.push(']');
                }
            }
            Some(c) => val.push(c),
            None => {
                scanner.source = checkpoint;
                return Some(Category::Error(ScanError::UnexpectedEof));
            }
        }
    }
    Some(Category::String(val))
}

fn skip_line(scanner: &mut Scanner) {
    while let Some(c) = scanner.advance() {
        if c == '\n' {
            break;
        }
    }
}

fn skip_insignificant(scanner: &mut Scanner) {
    while let Some(lookahead) = scanner.peek() {
        if is_whitespace(lookahead) {
            scanner.advance();
        } else if scanner.source.as_str().starts_with("--") {
            scanner.advance();
            scanner.advance();
            match scanner.peek() {
                Some('[') => {
                    match_long_comment(scanner);
                }
                Some(_) => {
                    skip_line(scanner);
                }
                None => break,
            }
        } else {
            break;
        }
    }
}

impl Scanner<'_> {
    pub fn next(&mut self) -> Token {
        skip_insignificant(self);
        let lookahead = if let Some(c) = self.advance() {
            c
        } else {
            return self.emit(Category::Eof);
        };
        match lookahead {
            '+' => self.emit(Category::Plus),
            '-' => self.emit(Category::Minus),
            '*' => self.emit(Category::Star),
            '/' => self.advance_if_eq_else('/', Category::Slash2, Category::Slash),
            '%' => self.emit(Category::Percent),
            '^' => self.emit(Category::Caret),
            '#' => self.emit(Category::Hash),
            '&' => self.emit(Category::Ampersand),
            '~' => self.advance_if_eq_else('=', Category::TildeEqual, Category::Tilde),
            '|' => self.emit(Category::Pipe),
            '<' => match self.peek() {
                Some('=') => {
                    self.advance();
                    self.emit(Category::LessThanEqual)
                }
                Some('<') => {
                    self.advance();
                    self.emit(Category::LessThan2)
                }
                _ => self.emit(Category::LessThan),
            },
            '>' => match self.peek() {
                Some('=') => {
                    self.advance();
                    self.emit(Category::GreaterThanEqual)
                }
                Some('>') => {
                    self.advance();
                    self.emit(Category::GreaterThan2)
                }
                _ => self.emit(Category::GreaterThan),
            },
            '=' => self.advance_if_eq_else('=', Category::Equal2, Category::Equal),
            '(' => self.emit(Category::LeftParen),
            ')' => self.emit(Category::RightParen),
            '{' => self.emit(Category::LeftBrace),
            '}' => self.emit(Category::RightBrace),
            '[' => {
                if let Some(tok) = resume_long_literal_string(self) {
                    self.emit(tok)
                } else {
                    self.emit(Category::LeftBracket)
                }
            }
            ']' => self.emit(Category::RightBracket),
            ':' => self.advance_if_eq_else(':', Category::Colon2, Category::Colon),
            ';' => self.emit(Category::SemiColon),
            ',' => self.emit(Category::Comma),

            // TODO what if number? Ex: .5e-1
            // TODO double check number of symbols consumed '.':
            '.' => match self.peek() {
                Some('.') => {
                    self.advance();
                    self.advance_if_eq_else('.', Category::Dot3, Category::Dot2)
                }
                Some(c) => {
                    if c.is_ascii_digit() {
                        match_numeral(self, '.')
                    } else {
                        self.emit(Category::Dot)
                    }
                }
                _ => self.emit(Category::Dot),
            },
            '\'' => match_string(self, '\''),
            '"' => match_string(self, '"'),
            _ => {
                if lookahead.is_ascii_alphabetic() || lookahead == '_' {
                    match_name(self, lookahead)
                } else if lookahead.is_ascii_digit() {
                    match_numeral(self, lookahead)
                } else {
                    self.emit(Category::Error(ScanError::UnexpectedCharacter(lookahead)))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn categories(source: &str) -> Vec<Category> {
        let mut scanner = Scanner::new(source);
        let mut cats = Vec::new();
        loop {
            let token = scanner.next();
            if Category::Eof == token.category {
                return cats;
            } else {
                cats.push(token.category);
            }
        }
    }

    #[test]
    fn test_match_long_comment() {
        assert_eq!(categories("--[[test]] 1"), vec![Category::Integer(1)],)
    }

    #[test]
    fn test_match_numeral() {
        assert_eq!(
            categories("1 1.0 1e0 1E+0 1e-0 1. .1 1.e-1 .1e0 0x10"),
            vec![
                Category::Integer(1),
                Category::Float(1.0),
                Category::Float(1.0),
                Category::Float(1.0),
                Category::Float(1.0),
                Category::Float(1.0),
                Category::Float(0.1),
                Category::Float(0.1),
                Category::Float(0.1),
                Category::Integer(16),
            ],
        )
    }

    #[test]
    fn test_match_string() {
        assert_eq!(
            categories("\"\\abcdefg\\\"\""),
            vec![Category::String("\x07bcdefg\"".to_string()),],
        )
    }
}
