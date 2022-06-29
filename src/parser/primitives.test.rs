use crate::parser::{
    parse_binary_expression, parse_for_loop, parse_identifier, parse_let_binding, parse_literal,
    parse_number, parse_number_as_expr, parse_while_loop, BinaryExpr, BinaryOperator, Expr,
    ForLoop, Identifier, IfBlock, LetBinding, LiteralExpr, Number, Parser, Reassignment,
    StringLiteral, WhileLoop,
};

use super::{parse_grouping_expr, parse_if_block, parse_print_statement, parse_string_literal};
use crate::grammar::{Declaration, Statement};

#[test]
fn test_parse_number() {
    let src = "12 foo";
    let result = parse_number().parse(src).unwrap();

    assert_eq!((" foo", "12"), result);
}

#[test]
fn test_parse_number_float() {
    let parser = parse_number();

    let src = "12.34";
    let result = parser.parse(src);
    assert_eq!(Ok(("", "12.34")), result);
    assert!(parser.parse("12.34.34").is_err());

    let as_expr = parse_number_as_expr().parse(src);
    assert_eq!(
        Ok((
            "",
            Expr::Literal(LiteralExpr::NumberLiteral(Number::Float64(12.34)))
        )),
        as_expr
    );
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
            ";",
            LetBinding {
                identifier: Identifier(String::from("foo")),
                rhs: Expr::Binary(BinaryExpr {
                    lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                        Number::Integer32(1)
                    ))),
                    op: BinaryOperator::Plus,
                    rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                        Number::Integer32(2)
                    ))),
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
                lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                    Number::Integer32(1)
                ))),
                op: BinaryOperator::Plus,
                rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                    Number::Integer32(2)
                ))),
            })))
        )),
        parser.parse("(1+2)")
    );

    assert_eq!(
        Ok((
            "",
            Expr::Grouping(Box::new(Expr::Grouping(Box::new(Expr::Literal(
                LiteralExpr::NumberLiteral(Number::Integer32(1))
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
                    lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                        Number::Integer32(1)
                    ))),
                    op: BinaryOperator::Plus,
                    rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                        Number::Integer32(2)
                    ))),
                })))),
                op: BinaryOperator::Mul,
                rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                    Number::Integer32(3)
                )))
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
                lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                    Number::Integer32(1)
                ))),
                op: BinaryOperator::Plus,
                rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                    Number::Integer32(2)
                ))),
            })
        )),
        parser.parse("print 1+2;")
    );
}

#[test]
fn test_parse_if_block() {
    let result = parse_if_block(
        "if (1) {
        print 2;
    }",
    );

    assert_eq!(
        Ok((
            "",
            IfBlock {
                cond: Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(1))),
                truthy_statement: Statement::Block(vec![Declaration::Statement(Statement::Print(
                    Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(2)))
                ))]),
                else_statement: None
            }
        )),
        result
    );
}

#[test]
fn test_parse_if_block_with_else() {
    let result = parse_if_block(
        "
        if (1) {
            print 2;
        } else {
            print 3;
        }
        ",
    );

    assert_eq!(
        Ok((
            "",
            IfBlock {
                cond: Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(1))),
                truthy_statement: Statement::Block(vec![Declaration::Statement(Statement::Print(
                    Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(2)))
                ))]),
                else_statement: Some(Statement::Block(vec![Declaration::Statement(
                    Statement::Print(Expr::Literal(LiteralExpr::NumberLiteral(
                        Number::Integer32(3)
                    )))
                )]))
            }
        )),
        result
    );
}

#[test]
fn test_parse_string_literal() {
    let parser = parse_string_literal();
    let src = r#""Hello, world!""#;

    assert_eq!(
        Ok((
            "",
            Expr::Literal(LiteralExpr::StringLiteral(StringLiteral(String::from(
                "Hello, world!"
            ))))
        )),
        parser.parse(src)
    );
}

#[test]
fn test_parse_while_loop() {
    assert_eq!(
        Ok((
            "",
            WhileLoop {
                condition: Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(1))),
                body: Statement::Block(vec![Declaration::Statement(Statement::Print(
                    Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(123)))
                ))])
            }
        )),
        parse_while_loop("while (1) { print 123; }")
    );
}

#[test]
fn test_parse_for_loop() {
    assert_eq!(
        Ok((
            "",
            ForLoop {
                init_declaration: Declaration::Let(LetBinding {
                    identifier: Identifier(String::from("k")),
                    rhs: Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(0))),
                }),
                condition: Expr::Binary(BinaryExpr {
                    lhs: Box::new(Expr::Literal(LiteralExpr::Identifier(Identifier(
                        String::from("k")
                    )))),
                    op: BinaryOperator::Less,
                    rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                        Number::Integer32(10)
                    ))),
                }),
                post_iteration: Declaration::Reassignment(Reassignment {
                    identifier: Identifier(String::from("k")),
                    rhs: Expr::Binary(BinaryExpr {
                        lhs: Box::new(Expr::Literal(LiteralExpr::Identifier(Identifier(
                            String::from("k")
                        )))),
                        op: BinaryOperator::Plus,
                        rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                            Number::Integer32(1)
                        ))),
                    })
                }),
                body: Statement::Block(vec![Declaration::Statement(Statement::Print(
                    Expr::Literal(LiteralExpr::Identifier(Identifier(String::from("a"))))
                ))])
            }
        )),
        parse_for_loop("for (let k = 0; k < 10; k = k + 1) { print a; }")
    );
}
