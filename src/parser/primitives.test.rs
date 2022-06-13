use crate::parser::{parse_identifier, parse_literal, parse_number, Parser};

#[test]
fn test_parse_number() {
    let src = "12 foo";
    let result = parse_number().parse(src).unwrap();

    assert_eq!((" foo", "12"), result);
}

#[test]
fn test_parse_identifier() {
    let src = "foo_bar123";
    let result = parse_identifier().parse(src).unwrap();

    assert_eq!(("", "foo_bar123"), result);

    assert!(parse_identifier().parse("123foo_bar").is_err());
    assert!(parse_identifier().parse("_foo_bar123").is_ok());
}

#[test]
fn test_parse_lparen() {
    let lparen_parser = parse_literal("(");

    let src = "(12 + 3)";
    let result = lparen_parser.parse(src).unwrap();

    assert_eq!(("12 + 3)", "("), result);
}
