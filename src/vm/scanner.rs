use std::str;

#[derive(Clone, PartialEq, Debug, PartialOrd)]
pub enum TokenKind {
    // primitive types
    Identifier(String),
    Number(f64),

    // semantics
    LParen,
    RParen,
    LBrace,
    RBrace,
    LSquare,
    RSquare,

    Semicolon,
    Comma,
    Dot,

    // basic ops
    Plus,
    Minus,
    Slash,
    Asterisk,

    // one or two char tokens
    Eq,
    Bang,
    EqEq,
    BangEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,

    // reserved keywords
    AND,
    OR,

    IF,
    ELSE,
    FOR,
    WHILE,
    FUN,
    NIL,
    PRINT,
    RETURN,
    // SUPER, THIS, CLASS
    LET,

    TRUE,
    FALSE,

    COMMENT,
    EOF,
    Root,
}

pub struct Lexer<'a> {
    source: &'a str,
    pub position: usize,
    pub had_error: bool,
    pub panic_mode: bool,
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
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn consume(&mut self, token_kind: TokenKind) -> LexerResult<()> {
        match self.next_token() {
            Ok(Some(next_tok)) => {
                if next_tok.kind == token_kind {
                    Ok(())
                } else {
                    Err(format!(
                        "Expected {:?}, got {:?} @ {}",
                        token_kind, next_tok.kind, next_tok.index_start,
                    ))
                }
            }
            _ => Err(String::from("skap")),
        }
    }

    pub fn panic_mode_on(&mut self) {
        self.panic_mode = true;
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
            Err(String::from("LexerError::CouldNotTokenize"))
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

pub type LexerResult<T> = Result<T, String>;

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

pub fn tokenize_ident(src: &str) -> LexerResult<(TokenKind, usize)> {
    // identifiers cant start with a number
    match src.chars().next() {
        Some(ch) => {
            if ch.is_digit(10) {
                return Err(String::from("LexerError::IdentStartsWithANumber"));
            }
        }
        None => return Err(String::from("LexerError::UnexpectedEOF")),
    }

    let (received, bytes_read) = take_while(src, |ch| ch == '_' || ch.is_alphanumeric())?;

    // TODO: match reserved keywords
    let received_lowercase = received.to_lowercase();
    let token = match received_lowercase.as_str() {
        "let" => TokenKind::LET,
        "for" => TokenKind::FOR,
        "while" => TokenKind::WHILE,
        "if" => TokenKind::IF,
        "else" => TokenKind::ELSE,
        "fun" => TokenKind::FUN,
        "nil" => TokenKind::NIL,
        "print" => TokenKind::PRINT,
        "return" => TokenKind::RETURN,
        "true" => TokenKind::TRUE,
        "false" => TokenKind::FALSE,
        _ => TokenKind::Identifier(received.to_string()),
    };

    Ok((token, bytes_read))
}

fn tokenize_string_literal(src: &str) -> LexerResult<(TokenKind, usize)> {
    todo!();
}

pub fn tokenize_arbitrary_number(src: &str) -> LexerResult<(TokenKind, usize)> {
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

    let tok = {
        let dec: f64 = received.parse().map_err(|_| {
            format!(
                "LexerError::CouldNotParseType expected f64, got {:?}",
                received.to_string()
            )
        })?;
        TokenKind::Number(dec)
    };

    Ok((tok, bytes_read))
}

pub fn skip_while_has_whitespace(data: &str) -> usize {
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
        Err(String::from("LexerError::NoCharMatch"))
    } else {
        Ok((&data[..current_index], current_index))
    }
}

pub fn tokenize_single_token(data: &str) -> LexerResult<(TokenKind, usize)> {
    let mut chars = data.chars().peekable();
    let maybe_upcoming = chars.next();

    if let Some(upcoming) = maybe_upcoming {
        match upcoming {
            '+' => Ok((TokenKind::Plus, 1)),
            '-' => Ok((TokenKind::Minus, 1)),
            '*' => Ok((TokenKind::Asterisk, 1)),
            '(' => Ok((TokenKind::LParen, 1)),
            ')' => Ok((TokenKind::RParen, 1)),
            '{' => Ok((TokenKind::LBrace, 1)),
            '}' => Ok((TokenKind::RBrace, 1)),
            '[' => Ok((TokenKind::LSquare, 1)),
            ']' => Ok((TokenKind::RSquare, 1)),
            ';' => Ok((TokenKind::Semicolon, 1)),
            '!' => {
                if let Some(nxt) = chars.peek() {
                    match nxt {
                        '=' => Ok((TokenKind::BangEq, 2)),
                        _ => Ok((TokenKind::Bang, 1)),
                    }
                } else {
                    Ok((TokenKind::EOF, 1))
                }
            }
            '=' => {
                if let Some(nxt) = chars.peek() {
                    match nxt {
                        '=' => Ok((TokenKind::EqEq, 2)),
                        _ => Ok((TokenKind::Eq, 1)),
                    }
                } else {
                    Ok((TokenKind::EOF, 1))
                }
            }
            '<' => {
                if let Some(nxt) = chars.peek() {
                    match nxt {
                        '=' => Ok((TokenKind::LessEq, 2)),
                        _ => Ok((TokenKind::Less, 1)),
                    }
                } else {
                    Ok((TokenKind::EOF, 1))
                }
            }
            '>' => {
                if let Some(nxt) = chars.peek() {
                    match nxt {
                        '=' => Ok((TokenKind::GreaterEq, 2)),
                        _ => Ok((TokenKind::Greater, 1)),
                    }
                } else {
                    Ok((TokenKind::EOF, 1))
                }
            }
            '/' => {
                if let Some(nxt) = chars.peek() {
                    if *nxt == '/' {
                        let _ = chars.next();
                        take_while(&data[2..], |ch| ch != '\n')
                            .map(|(_, bytes_read)| (TokenKind::COMMENT, 2 + bytes_read))
                    } else {
                        Ok((TokenKind::Slash, 1))
                    }
                } else {
                    Ok((TokenKind::Slash, 1))
                }
            }
            '0'..='9' => tokenize_arbitrary_number(data),
            c @ '_' | c if c.is_alphabetic() => tokenize_ident(data),
            _ => Err(String::from("LexerError::UnsupportedChar(upcoming)")),
        }
    } else {
        Ok((TokenKind::EOF, 1))
    }
}

pub fn tokenize(src: &str) -> LexerResult<Vec<LexedToken>> {
    let mut tokenizer = Lexer::new(src);
    let mut tokens = vec![];

    while let Ok(Some(lexed_token)) = tokenizer.next_token() {
        tokens.push(lexed_token);
    }

    tokens.push(LexedToken {
        kind: TokenKind::EOF,
        index_start: if tokens.len() > 0 {
            tokens.last().unwrap().index_start + 1
        } else {
            0
        },
    });

    Ok(tokens)
}

#[cfg(test)]
#[path = "scanner.test.rs"]
mod tests;
