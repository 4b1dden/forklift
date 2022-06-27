use super::{parse_declaration, parse_statement, Declaration};

use crate::grammar::Statement;
use crate::parser::{
    zero_or_more, Expr, Identifier, IfBlock, LetBinding, LiteralExpr, Number, Parser,
};

#[test]
fn test_parse_declaration() {
    let multi = zero_or_more(parse_declaration);

    assert_eq!(
        multi.parse("let a = 2;\n   let b=3;"),
        Ok((
            "",
            vec![
                Declaration::Let(LetBinding {
                    identifier: Identifier(String::from("a")),
                    rhs: Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(2))),
                }),
                Declaration::Let(LetBinding {
                    identifier: Identifier(String::from("b")),
                    rhs: Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(3))),
                })
            ]
        ))
    );
}

#[test]
fn test_parse_statement() {
    let parser = parse_statement();

    assert_eq!(
        Ok((
            "",
            Statement::If(Box::new(IfBlock {
                cond: Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(1))),
                truthy_statement: Statement::Block(vec![Declaration::Statement(Statement::Print(
                    Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(10)))
                ))]),
                else_statement: None
            }))
        )),
        parser.parse(
            "if (1) {
        print 10;
    }"
        )
    );
}
