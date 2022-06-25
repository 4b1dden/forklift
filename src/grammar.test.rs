use super::{parse_declaration, parse_statement, Declaration};

use crate::parser::{zero_or_more, Expr, Identifier, LetBinding, LiteralExpr, Number, Parser};

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
                    rhs: Expr::Literal(LiteralExpr::NumberLiteral(Number(2))),
                }),
                Declaration::Let(LetBinding {
                    identifier: Identifier(String::from("b")),
                    rhs: Expr::Literal(LiteralExpr::NumberLiteral(Number(3))),
                })
            ]
        ))
    );
}

/*
#[test]
fn test_parse_statement() {
    let parser = parse_statement();

    println!(
        "{:#?}",
        parser.parse(
            "if (1) {
        let c = 10;
        print c;
    }"
        )
    );
}
*/
