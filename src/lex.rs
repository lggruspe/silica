use std::str;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Name(String), // lexeme

    // keywords
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
    Error(ScanError),
}

pub struct Scanner<'a> {
    pub source: str::Chars<'a>,
    pub line_number: u64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ScanError {
    InvalidEscape,
    UnexpectedCharacter(char),
    UnexpectedEOF,
    MalformedNumber,
}

fn match_keyword(name: &str) -> Option<Token> {
    match name {
        "and" => Some(Token::And),
        "break" => Some(Token::Break),
        "do" => Some(Token::Do),
        "else" => Some(Token::Else),
        "elseif" => Some(Token::ElseIf),
        "end" => Some(Token::End),
        "false" => Some(Token::False),
        "for" => Some(Token::For),
        "function" => Some(Token::Function),
        "goto" => Some(Token::Goto),
        "if" => Some(Token::If),
        "in" => Some(Token::In),
        "local" => Some(Token::Local),
        "nil" => Some(Token::Nil),
        "not" => Some(Token::Not),
        "or" => Some(Token::Or),
        "repeat" => Some(Token::Repeat),
        "return" => Some(Token::Return),
        "then" => Some(Token::Then),
        "true" => Some(Token::True),
        "until" => Some(Token::Until),
        "while" => Some(Token::While),
        _ => None,
    }
}

fn match_name(scanner: &mut Scanner, prefix: char) -> Token {
    // latin alphanumeric chars or underscores, case-sensitive, non-keyword, doesn't start with digit
    let mut name = prefix.to_string();
    while let Some(c) = scanner.peek() {
        if c.is_ascii_alphanumeric() || c == '_' {
            scanner.advance().map(|a| name.push(a));
        } else {
            break;
        }
    }
    match_keyword(&name).unwrap_or(Token::Name(name))
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

fn match_number(scanner: &mut Scanner, prefix: char) -> Token {
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
        return Token::Error(ScanError::MalformedNumber);
    }
    if let Some(c) = scanner.peek() {
        if c == 'e' || c == 'E' {
            scanner.advance();
            lexeme.push(c);
            if let Some(exp) = resume_exponent(scanner) {
                lexeme += &exp;
                return Token::Float(lexeme.parse().unwrap());
            } else {
                return Token::Error(ScanError::MalformedNumber);
            }
        }
    }
    if points == 0 {
        Token::Integer(lexeme.parse().unwrap())
    } else {
        Token::Float(lexeme.parse().unwrap())
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

fn match_string(scanner: &mut Scanner, delim: char) -> Option<Token> {
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
            return Some(Token::Error(ScanError::UnexpectedEOF));
        };
        if c == delim {
            scanner.advance();
            return if has_invalid_escape {
                Some(Token::Error(ScanError::InvalidEscape))
            } else {
                Some(Token::String(string))
            };
        } else if c != '\\' {
            scanner.advance();
            string.push(c);
        } else {
            scanner.advance();
            match scanner.advance() {
                // TODO unicode, hex and digits
                Some('z') => skip_whitespace(scanner),
                None => return Some(Token::Error(ScanError::UnexpectedEOF)),
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
    fn new(source: &'a str) -> Self {
        let mut scanner = Scanner {
            source: source.chars(),
            line_number: 1,
        };
        if scanner.source.as_str().starts_with("#!") {
            skip_line(&mut scanner);
        }
        scanner
    }

    fn advance(&mut self) -> Option<char> {
        self.source.next()
    }

    fn advance_if_eq_else(&mut self, c: char, a: Token, b: Token) -> Token {
        match self.peek() {
            None => b,
            Some(p) => {
                if p == c {
                    self.advance();
                    a
                } else {
                    b
                }
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.clone().nth(0)
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
        if lookahead == '\n' {
            scanner.line_number += 1;
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
        if c == '\n' {
            scanner.line_number += 1;
        } else if c == ']' && resume_closing_long_bracket(scanner, level) {
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
fn resume_long_literal_string(scanner: &mut Scanner) -> Option<Token> {
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
            Some('\n') => {
                scanner.line_number += 1;
                if let Some('\r') = scanner.peek() {
                    scanner.advance();
                }
                val.push('\n');
            }
            Some('\r') => {
                scanner.line_number += 1;
                if let Some('\n') = scanner.peek() {
                    scanner.advance();
                }
                val.push('\n');
            }
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
                return Some(Token::Error(ScanError::UnexpectedEOF));
            }
        }
    }
    Some(Token::String(val))
}

fn skip_line(scanner: &mut Scanner) {
    while let Some(c) = scanner.advance() {
        if c == '\n' {
            scanner.line_number += 1;
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

impl Iterator for Scanner<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        skip_insignificant(self);
        let lookahead = self.advance()?;
        match lookahead {
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '*' => Some(Token::Star),
            '/' => Some(self.advance_if_eq_else('/', Token::Slash2, Token::Slash)),
            '%' => Some(Token::Percent),
            '^' => Some(Token::Caret),
            '#' => Some(Token::Hash),
            '&' => Some(Token::Ampersand),
            '~' => Some(self.advance_if_eq_else('=', Token::TildeEqual, Token::Tilde)),
            '|' => Some(Token::Pipe),
            '<' => match self.peek() {
                Some('=') => {
                    self.advance();
                    Some(Token::LessThanEqual)
                }
                Some('<') => {
                    self.advance();
                    Some(Token::LessThan2)
                }
                _ => Some(Token::LessThan),
            },
            '>' => match self.peek() {
                Some('=') => {
                    self.advance();
                    Some(Token::GreaterThanEqual)
                }
                Some('>') => {
                    self.advance();
                    Some(Token::GreaterThan2)
                }
                _ => Some(Token::GreaterThan),
            },
            '=' => Some(self.advance_if_eq_else('=', Token::Equal2, Token::Equal)),
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            '{' => Some(Token::LeftBrace),
            '}' => Some(Token::RightBrace),
            '[' => {
                if let Some(tok) = resume_long_literal_string(self) {
                    Some(tok)
                } else {
                    Some(Token::LeftBracket)
                }
            }
            ']' => Some(Token::RightBracket),
            ':' => Some(self.advance_if_eq_else(':', Token::Colon2, Token::Colon)),
            ';' => Some(Token::SemiColon),
            ',' => Some(Token::Comma),

            // TODO what if number? Ex: .5e-1
            // TODO double check number of symbols consumed '.':
            '.' => match self.peek() {
                Some('.') => {
                    self.advance();
                    Some(self.advance_if_eq_else('.', Token::Dot3, Token::Dot2))
                }
                Some(c) => {
                    if c.is_ascii_digit() {
                        Some(match_number(self, '.'))
                    } else {
                        Some(Token::Dot)
                    }
                }
                _ => Some(Token::Dot),
            },
            '\'' => match_string(self, '\''),
            '"' => match_string(self, '"'),
            _ => {
                if lookahead.is_ascii_alphabetic() || lookahead == '_' {
                    Some(match_name(self, lookahead))
                } else if lookahead.is_ascii_digit() {
                    Some(match_number(self, lookahead))
                } else {
                    Some(Token::Error(ScanError::UnexpectedCharacter(lookahead)))
                }
            }
        }
    }
}

pub fn scan(source: &str) -> Scanner {
    Scanner::new(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_match_long_comment() {
        assert_eq!(
            scan("--[[test]] 1").collect::<Vec<Token>>(),
            vec![Token::Integer(1)],
        )
    }

    #[test]
    fn test_match_number() {
        assert_eq!(
            scan("1 1.0 1e0 1E+0 1e-0 1. .1 1.e-1 .1e0").collect::<Vec<Token>>(),
            vec![
                Token::Integer(1),
                Token::Float(1.0),
                Token::Float(1.0),
                Token::Float(1.0),
                Token::Float(1.0),
                Token::Float(1.0),
                Token::Float(0.1),
                Token::Float(0.1),
                Token::Float(0.1),
            ],
        )
    }

    #[test]
    fn test_match_string() {
        assert_eq!(
            scan("\"\\abcdefg\\\"\"").collect::<Vec<Token>>(),
            vec![Token::String("\x07bcdefg\"".to_string()),],
        )
    }
}
