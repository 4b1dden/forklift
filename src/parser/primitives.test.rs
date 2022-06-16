use crate::parser::{
    parse_identifier, parse_let_binding, parse_literal, parse_number, BinaryExpr, BinaryOperator,
    Expr, Identifier, LetAssignment, LiteralExpr, Number, Parser,
};

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

#[test]
fn test_parse_let_binding() {
    let src = "let foo = 1 + 2";
    let parser = parse_let_binding();

    assert_eq!(
        parser.parse(src).unwrap(),
        (
            "",
            LetAssignment {
                identifier: Expr::Literal(LiteralExpr::Identifier(Identifier(String::from("foo")))),
                rhs: Expr::Binary(BinaryExpr {
                    lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(1)))),
                    op: BinaryOperator::Plus,
                    rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(2)))),
                })
            }
        )
    );

    assert_eq!(
        parser.parse("let foo  1 + 2"),
        Err(String::from("expected =, got 1"))
    );
    assert_eq!(
        parser.parse("foo = 1 + 2"),
        Err(String::from("expected let, got foo"))
    );
}
