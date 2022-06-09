use std::str;

#[derive(Clone, PartialEq, Debug, PartialOrd)]
pub enum TokenKind {
    // primitive types
    Identifier(String),
    Integer(i32),
    Decimal(f64),

    // semantics
    LParen,
    RParen,
    LBrace,
    RBrace,
    LSquare,
    RSquare,
    Semicolon,
    Eq,

    // basic ops
    Plus,
    Minus,
    Slash,
    Asterisk,
    Bang,

    Root,
}

pub struct Lexer<'a> {
    source: &'a str,
    pub position: usize,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct LexedToken {
    pub kind: TokenKind,
    pub index_start: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> LexerResult<Option<LexedToken>> {
        self.skip_whitespaces();

        if self.source.is_empty() {
            Ok(None)
        } else {
            let start = self.position;
            let (tok, bytes_skipped) = self._next_token()?;
            let end = start + bytes_skipped;

            Ok(Some(LexedToken {
                kind: tok,
                index_start: start,
            }))
        }
    }

    fn _next_token(&mut self) -> LexerResult<(TokenKind, usize)> {
        let snippet_err_offset = 20;
        if let Ok((tok, bytes_skipped)) = tokenize_single_token(self.source) {
            self.move_forward(bytes_skipped);
            Ok((tok, bytes_skipped))
        } else {
            Err(LexerError::CouldNotTokenize {
                position: self.position,
                snippet: self.source[..snippet_err_offset].to_owned(),
            })
        }
    }

    fn skip_whitespaces(&mut self) {
        let bytes_skipped = skip_while_has_whitespace(self.source);
        self.move_forward(bytes_skipped);
    }

    fn move_forward(&mut self, num_bytes: usize) {
        self.source = &self.source[num_bytes..];
        self.position += num_bytes;
    }
}

pub type LexerResult<T> = Result<T, LexerError>;

#[derive(PartialEq, Debug)]
pub enum LexerError {
    IdentStartsWithANumber,
    UnexpectedEOF,
    NoCharMatch,
    MultipleDotsInNumber,
    UnsupportedChar(char),
    CouldNotTokenize {
        position: usize,
        snippet: String,
    },
    CouldNotParseType {
        expected_type: String, // we'll introduce types enum later
        received: String,
    },
}

fn tokenize_ident(src: &str) -> LexerResult<(TokenKind, usize)> {
    // identifiers cant start with a number
    match src.chars().next() {
        Some(ch) => {
            if ch.is_digit(10) {
                return Err(LexerError::IdentStartsWithANumber);
            }
        }
        None => return Err(LexerError::UnexpectedEOF),
    }

    let (received, bytes_read) = take_while(src, |ch| ch == '_' || ch.is_alphanumeric())?;

    // TODO: match reserved keywords

    let token = TokenKind::Identifier(received.to_string());

    Ok((token, bytes_read))
}

fn tokenize_string_literal(src: &str) -> LexerResult<(TokenKind, usize)> {
    todo!();
}

fn tokenize_arbitrary_number(src: &str) -> LexerResult<(TokenKind, usize)> {
    let mut has_dot = false;

    let (received, bytes_read) = take_while(src, |ch| {
        if ch.is_digit(10) {
            true
        } else {
            if ch == '.' {
                if !has_dot {
                    has_dot = true;
                    true
                } else {
                    false
                    // return Err(LexerError::MultipleDotsInNumber);
                }
            } else {
                false
                // return Err(LexerError::AmbiguousCharInNumber);
            }
        }
    })?;

    let tok = if has_dot {
        let dec: f64 = received
            .parse()
            .map_err(|_| LexerError::CouldNotParseType {
                expected_type: String::from("f64"),
                received: received.to_string(),
            })?;
        TokenKind::Decimal(dec)
    } else {
        let int: i32 = received
            .parse()
            .map_err(|_| LexerError::CouldNotParseType {
                expected_type: String::from("i32"),
                received: received.to_string(),
            })?;
        TokenKind::Integer(int)
    };

    Ok((tok, bytes_read))
}

fn skip_while_has_whitespace(data: &str) -> usize {
    match take_while(data, |ch| ch.is_whitespace()) {
        Ok((_, bytes_read)) => bytes_read,
        Err(_) => 0,
    }
}

fn take_while<F>(data: &str, mut pred: F) -> LexerResult<(&str, usize)>
where
    F: FnMut(char) -> bool,
{
    let mut current_index = 0;

    for ch in data.chars() {
        let should_continue = pred(ch);

        if !should_continue {
            break;
        }

        current_index += ch.len_utf8();
    }

    if current_index == 0 {
        Err(LexerError::NoCharMatch)
    } else {
        Ok((&data[..current_index], current_index))
    }
}

pub fn tokenize_single_token(data: &str) -> LexerResult<(TokenKind, usize)> {
    let upcoming = data.chars().next().ok_or(LexerError::UnexpectedEOF)?;

    match upcoming {
        '+' => Ok((TokenKind::Plus, 1)),
        '-' => Ok((TokenKind::Minus, 1)),
        '*' => Ok((TokenKind::Asterisk, 1)),
        '/' => Ok((TokenKind::Slash, 1)),
        '(' => Ok((TokenKind::LParen, 1)),
        ')' => Ok((TokenKind::RParen, 1)),
        '{' => Ok((TokenKind::LBrace, 1)),
        '}' => Ok((TokenKind::RBrace, 1)),
        '[' => Ok((TokenKind::LSquare, 1)),
        ']' => Ok((TokenKind::RSquare, 1)),
        ';' => Ok((TokenKind::Semicolon, 1)),
        '=' => Ok((TokenKind::Eq, 1)),
        '0'..='9' => tokenize_arbitrary_number(data),
        c @ '_' | c if c.is_alphabetic() => tokenize_ident(data),
        _ => Err(LexerError::UnsupportedChar(upcoming)),
    }
}

pub fn tokenize(src: &str) -> LexerResult<Vec<LexedToken>> {
    let mut tokenizer = Lexer::new(src);
    let mut tokens = vec![];

    while let Some(lexed_token) = tokenizer.next_token()? {
        tokens.push(lexed_token);
    }

    Ok(tokens)
}

#[cfg(test)]
#[test]
fn test_tokenize_ident() {
    let src = String::from("foo");
    let result = tokenize_ident(&src);

    let expected = Ok((TokenKind::Identifier(String::from("foo")), 3));
    assert_eq!(result, expected);
}

#[cfg(test)]
#[test]
fn test_tokenize_ident_with_underscore() {
    let src = String::from("foo_bar");
    let result = tokenize_ident(&src);

    let expected = Ok((TokenKind::Identifier(String::from("foo_bar")), 7));
    assert_eq!(result, expected);
}

#[cfg(test)]
#[test]
fn test_tokenize_ident_error() {
    let src = String::from("12foo");
    let result = tokenize_ident(&src);

    let expected = Err(LexerError::IdentStartsWithANumber);
    assert_eq!(result, expected);
}

#[cfg(test)]
#[test]
fn test_tokenize_arbitrary_number() {
    let src1 = String::from("12.45");
    let expected1 = (TokenKind::Decimal(12.45), 5);
    let result1 = tokenize_arbitrary_number(&src1).unwrap();

    assert_eq!(result1, expected1);

    let src2 = String::from("6");
    let expected2 = (TokenKind::Integer(6), 1);
    let result2 = tokenize_arbitrary_number(&src2).unwrap();

    assert_eq!(result2, expected2);

    let src3 = String::from("100.45");
    let result3 = tokenize_arbitrary_number(&src3).unwrap();
    let expected3 = (TokenKind::Decimal(100.45), 6);

    assert_eq!(result3, expected3);
}

#[cfg(test)]
#[test]
fn test_tokenize_arbitrary_number_error() {
    // tODO
}

#[cfg(test)]
#[test]
fn test_skip_while_has_whitespace() {
    let src = "\n\tfoo";
    let result = skip_while_has_whitespace(&src);

    let expected = 2;

    assert_eq!(result, expected)
}

#[cfg(test)]
#[test]
fn test_skip_while_has_whitespace_plain_space() {
    let src = "   foo = bar";
    let result = skip_while_has_whitespace(&src);

    let expected = 3;

    assert_eq!(result, expected)
}

#[cfg(test)]
#[test]
fn test_tokenize_single_token() {
    assert_eq!(tokenize_single_token("+"), Ok((TokenKind::Plus, 1)));
    assert_eq!(tokenize_single_token("-"), Ok((TokenKind::Minus, 1)));
    assert_eq!(tokenize_single_token("*"), Ok((TokenKind::Asterisk, 1)));
    assert_eq!(tokenize_single_token("/"), Ok((TokenKind::Slash, 1)));
    assert_eq!(tokenize_single_token("{"), Ok((TokenKind::LBrace, 1)));
    assert_eq!(tokenize_single_token("}"), Ok((TokenKind::RBrace, 1)));
    assert_eq!(tokenize_single_token("["), Ok((TokenKind::LSquare, 1)));
    assert_eq!(tokenize_single_token("]"), Ok((TokenKind::RSquare, 1)));
    assert_eq!(tokenize_single_token("("), Ok((TokenKind::LParen, 1)));
    assert_eq!(tokenize_single_token(")"), Ok((TokenKind::RParen, 1)));
}

#[cfg(test)]
#[test]
fn test_tokenize_single_token_special_cases() {
    assert_eq!(
        tokenize_single_token("123"),
        Ok((TokenKind::Integer(123), 3))
    );
    assert_eq!(
        tokenize_single_token("123.45"),
        Ok((TokenKind::Decimal(123.45), 6))
    );
}
