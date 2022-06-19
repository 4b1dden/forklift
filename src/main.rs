use std::env;
use std::fs;
use std::io::{self, BufRead};
use std::path::Path;
use std::process::exit;

mod grammar;
mod parser;
mod tokenizer;

use crate::grammar::parse_declaration;
use crate::parser::{BoxedParser, Expr, Parser};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_help();
        exit(1);
    }

    let mode = &args[1];
    if mode == "repl" {
        run_repl_loop();
    } else if mode == "load" {
        let filepath = Path::new(&args[2]);
        load_and_eval_file(filepath);
    } else {
        println!("Unknown mode, use repl | load");
        exit(1);
    }
}

fn load_and_eval_file(path: &Path) {
    let contents = fs::read_to_string(path).expect("Filepath has to be valid");

    let program_parser = grammar::parse_program();
    let parsed_program = program_parser.parse(&contents);

    println!("{:#?}", parsed_program);
}

fn run_repl_loop() {
    let mut line = String::new();
    let stdin = io::stdin();

    loop {
        println!("fl >>>");
        line.clear();
        stdin.lock().read_line(&mut line).unwrap();

        if line.trim().to_lowercase() != "exit" {
            let dec = parse_declaration().parse(&line);
            println!("{:#?}", dec);
        } else {
            break;
        }
    }
}

fn print_help() {
    println!(
        "
        Usage: 
            [0] [required]          repl | load
            [1] [if --mode==load]   filepath
    "
    );
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
