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
    let mut lexeme = "".to_string();
    loop {
        match scanner.peek() {
            Some(c) => {
                if c.is_ascii_digit() {
                    lexeme.push(c);
                    scanner.advance();
                } else {
                    return lexeme;
                }
            }
            _ => return lexeme,
        }
    }
}

fn match_number(scanner: &mut Scanner, prefix: char) -> Token {
    // assumption: scanner.peek() is a digit
    // .4e-7 not covered
    // has optional fraction and optional decimal exponent
    // eE
    // also accepts hexadecimal constants (0x or 0X ...)
    // hex constants can have an optional fraction and exponent (pP ... instead of eE)
    // numbers with a radix point or an exponent are floats
    // otherwise, integer (if it fits, otherwise it's a float)

    let mut is_integer = true;
    let mut lexeme = prefix.to_string();
    lexeme += &match_digits(scanner);
    if let Some('.') = scanner.peek() {
        lexeme.push('.');
        scanner.advance();
        lexeme += &match_digits(scanner);
        is_integer = false;
    }
    if let Some(c) = scanner.peek() {
        if c == 'e' || c == 'E' {
            scanner.advance();
            lexeme.push(c);
            if let Some(c) = scanner.peek() {
                if c == '+' || c == '-' {
                    scanner.advance();
                    lexeme.push(c);
                }
            }
            let exponent = match_digits(scanner);
            if exponent.is_empty() {
                return Token::Error(ScanError::MalformedNumber);
            }
            lexeme += &exponent;
            is_integer = false;
        }
    }
    if is_integer {
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
    //
    // long literal strings delimited by [=[ (any number of '='s)
    // and closed by ]=] with the same number of '='s as opening delim
    // can have multple lines, doesn't interpret escape sequences
    // carriage return, newline, carriage return followed by a newline,
    // newline followed by a carriage return, etc. are all converted
    // to a simple newline
    // if the opening bracket is followed immediately by a newline,
    // the newline is excluded from the string

    let mut string = String::from("");
    let mut has_invalid_escape = false;
    loop {
        let c = scanner.peek();
        if let None = c {
            return Some(Token::Error(ScanError::UnexpectedEOF));
        }
        let c = c.unwrap();
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
        Scanner {
            source: source.chars(),
            line_number: 1,
        }
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

fn match_long_comment(scanner: &mut Scanner) {
    // assume -- has been consumed
    let first = scanner.advance();
    assert_eq!(first.unwrap(), '[');
    let mut level: i32 = 0;
    loop {
        match scanner.advance() {
            Some('=') => level += 1,
            Some('[') => break,
            Some('\n') => {
                scanner.line_number += 1;
                return;
            }
            _ => {
                level = -1;
                break;
            }
        }
    }
    if level < 0 {
        return skip_line(scanner);
    }
    let closing = "=".repeat(level as usize) + "]";
    while let Some(c) = scanner.advance() {
        if c == '\n' {
            scanner.line_number += 1;
        } else if c == ']' {
            if scanner.source.as_str().starts_with(&closing) {
                for _ in 0..level + 1 {
                    scanner.advance();
                }
                break;
            }
        }
    }
}

fn skip_line(scanner: &mut Scanner) {
    // assume -- has been consumed
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
            '[' => Some(Token::LeftBracket),
            ']' => Some(Token::RightBracket),
            ':' => Some(self.advance_if_eq_else(':', Token::Colon2, Token::Colon)),
            ';' => Some(Token::SemiColon),
            ',' => Some(Token::Comma),

            // TODO what if number? Ex: .5e-1
            // TODO double check number of symbols consumed '.':
            '.' => {
                if let Some('.') = self.peek() {
                    self.advance();
                    Some(self.advance_if_eq_else('.', Token::Dot3, Token::Dot2))
                } else {
                    Some(Token::Dot)
                }
            }
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
            scan("1 1.0 1e0 1E+0 1e-0").collect::<Vec<Token>>(),
            vec![
                Token::Integer(1),
                Token::Float(1.0),
                Token::Float(1.0),
                Token::Float(1.0),
                Token::Float(1.0),
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
