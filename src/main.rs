use std::io::{self, BufRead};

mod parser;
mod tokenizer;

use parser::{either, Expr};

use crate::parser::Parser;

fn parse_statement<'a>() -> impl Parser<'a, Expr> {
    either(
        parser::parse_binary_expression(),
        parser::parse_expr_literal(),
    )
}

fn main() {
    let mut line = String::new();
    let stdin = io::stdin();

    while true {
        println!("fl >>>");
        line.clear();
        stdin.lock().read_line(&mut line).unwrap();

        if line.trim().to_lowercase() != "exit" {
            let expr = parse_statement().parse(&line);
            println!("{:#?}", expr);
        } else {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, LexedToken, Lexer, LexerResult, TokenKind};
    fn mock_lexed_token(kind: TokenKind, start: usize) -> LexedToken {
        LexedToken {
            kind,
            index_start: start,
        }
    }

    #[test]
    fn test_tokenizer() {
        let source = "let foo = 1 + 2.52;";
        let result = tokenize(source).expect("lexer should not panic");

        let expected = vec![
            mock_lexed_token(TokenKind::Identifier(String::from("let")), 0),
            mock_lexed_token(TokenKind::Identifier(String::from("foo")), 4),
            mock_lexed_token(TokenKind::Eq, 8),
            mock_lexed_token(TokenKind::Integer(1), 10),
            mock_lexed_token(TokenKind::Plus, 12),
            mock_lexed_token(TokenKind::Decimal(2.52), 14),
            mock_lexed_token(TokenKind::Semicolon, 18),
        ];

        assert_eq!(result, expected);
    }

    #[test]
    fn test_tokenizer_with_parens() {
        let source = "let foo = (1 + 2) * 3;";
        let result = tokenize(source).expect("lexer should not panic");

        let expected = vec![
            mock_lexed_token(TokenKind::Identifier(String::from("let")), 0),
            mock_lexed_token(TokenKind::Identifier(String::from("foo")), 4),
            mock_lexed_token(TokenKind::Eq, 8),
            mock_lexed_token(TokenKind::LParen, 10),
            mock_lexed_token(TokenKind::Integer(1), 11),
            mock_lexed_token(TokenKind::Plus, 13),
            mock_lexed_token(TokenKind::Integer(2), 15),
            mock_lexed_token(TokenKind::RParen, 16),
            mock_lexed_token(TokenKind::Asterisk, 18),
            mock_lexed_token(TokenKind::Integer(3), 20),
            mock_lexed_token(TokenKind::Semicolon, 21),
        ];

        assert_eq!(result, expected);
    }
}
