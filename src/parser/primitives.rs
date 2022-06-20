use crate::parser::{
    and_then, at_least_one_whitespace, either, parse_binary_expression, parse_expr_literal,
    parse_unary_expression, sequence_of_monomorphic, trim_whitespace_around, BoxedParser, Expr,
    Identifier, Keywords, LiteralExpr, Number, Parser,
};

use super::{map, ParseResult};

pub fn parse_number<'a>() -> impl Parser<'a, &'a str> {
    move |input: &'a str| {
        let mut idx: usize = 0;

        while let Some(ch) = input.chars().nth(idx) {
            if ch.is_digit(10) {
                idx += 1;
            } else {
                break;
            }
        }

        Ok((&input[idx..], &input[..idx]))
    }
}

pub fn parse_identifier<'a>() -> impl Parser<'a, &'a str> {
    move |input: &'a str| {
        let mut matched_count: usize = 0;
        let mut chars = input.chars();

        if let Some(next) = chars.next() {
            if next.is_alphabetic() || next == '_' {
                matched_count += 1;
            } else {
                return Err(String::from(
                    "Identifier can not start with a non alphabetic character",
                ));
            }
        } else {
            return Err(String::from("UnexpectedEOF"));
        }

        while let Some(next) = chars.next() {
            if next.is_alphanumeric() || next == '_' {
                matched_count += 1;
            } else {
                break;
            }
        }

        Ok((&input[matched_count..], &input[..matched_count]))
    }
}

pub fn parse_identifier_as_expr<'a>() -> impl Parser<'a, Expr> {
    parse_identifier()
        .map(|ident| Expr::Literal(LiteralExpr::Identifier(Identifier(ident.to_string()))))
}

pub fn parse_number_as_expr<'a>() -> impl Parser<'a, Expr> {
    // TODO; rework to more functional and_then
    move |input| {
        let res = parse_number().parse(input);
        match res {
            Ok((rest, m)) => {
                let parsed: Result<i32, String> = m
                    .parse::<i32>()
                    .map_err(|_| String::from("Could not parse to i32"));
                match parsed {
                    Ok(num) => Ok((rest, Expr::Literal(LiteralExpr::NumberLiteral(Number(num))))),
                    Err(e) => Err(e),
                }
            }
            Err(e) => Err(e),
        }
    }
    //.map(|num| Expr::Literal(LiteralExpr::NumberLiteral(Number(num.parse().unwrap()))))
}

// TODO: extend to parse_keyword probably
pub fn parse_let_keyword<'a>() -> impl Parser<'a, Expr> {
    parse_literal("let").map(|literal_as_expr| Expr::Literal(LiteralExpr::Keyword(Keywords::Let)))
}

pub fn parse_literal<'a>(literal: &'a str) -> impl Parser<'a, &'a str> {
    move |input: &'a str| match input.get(0..literal.len()) {
        Some(substr) => {
            if substr == literal {
                Ok((&input[substr.len()..], &input[..substr.len()]))
            } else {
                Err(format!("expected {}, got {}", literal.to_string(), substr))
            }
        }
        None => Err("Unexpected EOF".to_string()),
    }
}

// TODO: figure out if this is the best structure
// maybe we want this to be a part of some wider "Statement" / "GrammarItem" enum?
#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding {
    pub identifier: Expr, // we only want Identifier type here, however, ..
    pub rhs: Expr,
}

// TODO: unify this with other "statement" parser in grammar
fn parse_statement<'a>() -> impl Parser<'a, Expr> {
    either(
        either(parse_unary_expression(), parse_binary_expression()),
        parse_expr_literal(),
    )
}

pub fn parse_let_binding<'a>() -> impl Parser<'a, LetBinding> {
    sequence_of_monomorphic(vec![
        BoxedParser::new(parse_let_keyword()),
        BoxedParser::new(at_least_one_whitespace().map(|_| Expr::Literal(LiteralExpr::Empty))),
        BoxedParser::new(parse_identifier_as_expr()),
        BoxedParser::new(
            trim_whitespace_around(parse_literal("=")).map(|_| Expr::Literal(LiteralExpr::Empty)),
        ),
        BoxedParser::new(parse_statement()),
        BoxedParser::new(parse_literal(";").map(|_| Expr::Literal(LiteralExpr::Empty))),
    ])
    .map(|results| LetBinding {
        identifier: results.get(2).unwrap().clone(),
        rhs: results.get(4).unwrap().clone(),
    })
}

#[cfg(test)]
#[path = "primitives.test.rs"]
mod tests;
