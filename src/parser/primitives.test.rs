use crate::parser::{
    parse_binary_expression, parse_identifier, parse_let_binding, parse_literal, parse_number,
    BinaryExpr, BinaryOperator, Expr, Identifier, LetBinding, LiteralExpr, Number, Parser,
};

use super::{parse_grouping_expr, parse_print_statement};

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
    let src = "let foo = 1 + 2;";
    let parser = parse_let_binding();

    assert_eq!(
        parser.parse(src).unwrap(),
        (
            "",
            LetBinding {
                identifier: Identifier(String::from("foo")),
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

#[test]
fn test_parse_grouping_expr() {
    let parser = parse_grouping_expr();

    assert_eq!(
        Ok((
            "",
            Expr::Grouping(Box::new(Expr::Binary(BinaryExpr {
                lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(1)))),
                op: BinaryOperator::Plus,
                rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(2)))),
            })))
        )),
        parser.parse("(1+2)")
    );

    assert_eq!(
        Ok((
            "",
            Expr::Grouping(Box::new(Expr::Grouping(Box::new(Expr::Literal(
                LiteralExpr::NumberLiteral(Number(1))
            )))))
        )),
        parser.parse("((1))"),
    );
}

#[test]
fn test_parse_binary_with_grouping() {
    let parser = parse_binary_expression();

    assert_eq!(
        Ok((
            "",
            Expr::Binary(BinaryExpr {
                lhs: Box::new(Expr::Grouping(Box::new(Expr::Binary(BinaryExpr {
                    lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(1)))),
                    op: BinaryOperator::Plus,
                    rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(2)))),
                })))),
                op: BinaryOperator::Mul,
                rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(3))))
            })
        )),
        parser.parse("(1+2)*3")
    );
}

#[test]
fn test_parse_print_statement() {
    let parser = parse_print_statement();

    assert_eq!(
        Ok((
            "",
            Expr::Binary(BinaryExpr {
                lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(1)))),
                op: BinaryOperator::Plus,
                rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(2)))),
            })
        )),
        parser.parse("print 1+2;")
    );
}
