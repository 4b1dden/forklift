use crate::vm::{
    skip_while_has_whitespace, tokenize, tokenize_arbitrary_number, tokenize_ident,
    tokenize_single_token, LexedToken, LexerError, TokenKind,
};

#[test]
fn test_tokenize_ident() {
    let src = String::from("foo");
    let result = tokenize_ident(&src);

    let expected = Ok((TokenKind::Identifier(String::from("foo")), 3));
    assert_eq!(result, expected);
}

#[test]
fn test_tokenize_ident_with_underscore() {
    let src = String::from("foo_bar");
    let result = tokenize_ident(&src);

    let expected = Ok((TokenKind::Identifier(String::from("foo_bar")), 7));
    assert_eq!(result, expected);
}

#[test]
fn test_tokenize_ident_error() {
    let src = String::from("12foo");
    let result = tokenize_ident(&src);

    let expected = Err(String::from("LexerError::IdentStartsWithANumber"));
    assert_eq!(result, expected);
}

#[test]
fn test_tokenize_arbitrary_number() {
    let src1 = String::from("12.45");
    let expected1 = (TokenKind::Number(12.45), 5);
    let result1 = tokenize_arbitrary_number(&src1).unwrap();

    assert_eq!(result1, expected1);

    let src2 = String::from("6");
    let expected2 = (TokenKind::Number(6_f64), 1);
    let result2 = tokenize_arbitrary_number(&src2).unwrap();

    assert_eq!(result2, expected2);

    let src3 = String::from("100.45");
    let result3 = tokenize_arbitrary_number(&src3).unwrap();
    let expected3 = (TokenKind::Number(100.45), 6);

    assert_eq!(result3, expected3);
}

#[test]
fn test_tokenize_arbitrary_number_error() {
    // tODO
}

#[test]
fn test_skip_while_has_whitespace() {
    let src = "\n\tfoo";
    let result = skip_while_has_whitespace(&src);

    let expected = 2;

    assert_eq!(result, expected)
}

#[test]
fn test_skip_while_has_whitespace_plain_space() {
    let src = "   foo = bar";
    let result = skip_while_has_whitespace(&src);

    let expected = 3;

    assert_eq!(result, expected)
}

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

#[test]
fn test_tokenize_single_token_special_cases() {
    assert_eq!(
        tokenize_single_token("123"),
        Ok((TokenKind::Number(123_f64), 3))
    );
    assert_eq!(
        tokenize_single_token("123.45"),
        Ok((TokenKind::Number(123.45), 6))
    );
}

#[test]
fn test_e2e() {
    assert!(check_order_only(
        tokenize("print a + b;").expect("Tokenizer should not fail"),
        vec![
            TokenKind::PRINT,
            TokenKind::Identifier(String::from("a")),
            TokenKind::Plus,
            TokenKind::Identifier(String::from("b")),
            TokenKind::Semicolon,
            TokenKind::EOF,
        ]
    ));

    assert!(check_order_only(
        tokenize("if(true) { foo(); }").expect("Tokenizer should not fail"),
        vec![
            TokenKind::IF,
            TokenKind::LParen,
            TokenKind::TRUE,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Identifier(String::from("foo")),
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Semicolon,
            TokenKind::RBrace,
            TokenKind::EOF,
        ]
    ));

    assert!(check_order_only(
        tokenize("let i = 0; for(;;) {}").expect("Tokenizer should not fail"),
        vec![
            TokenKind::LET,
            TokenKind::Identifier(String::from("i")),
            TokenKind::Eq,
            TokenKind::Number(0_f64),
            TokenKind::Semicolon,
            TokenKind::FOR,
            TokenKind::LParen,
            TokenKind::Semicolon,
            TokenKind::Semicolon,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::RBrace,
            TokenKind::EOF,
        ]
    ));

    assert!(check_order_only(
        tokenize("foo; + // bar").expect("Tokenizer should not fail"),
        vec![
            TokenKind::Identifier(String::from("foo")),
            TokenKind::Semicolon,
            TokenKind::Plus,
            TokenKind::COMMENT,
            TokenKind::EOF,
        ]
    ));
}

fn check_order_only(result: Vec<LexedToken>, expected: Vec<TokenKind>) -> bool {
    if result.len() != expected.len() {
        return false;
    }

    for (l, r) in result.iter().zip(expected.iter()) {
        if &l.kind != r {
            println!("mismatch at {:?} and {:?}", l.kind, r);
            return false;
        }
    }

    true
}
