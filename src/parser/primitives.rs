use crate::grammar::{parse_statement, wrapped_scope, Statement};
use crate::parser::{
    and_then, at_least_one_whitespace, either, parse_binary_expression, parse_expr_literal,
    parse_unary_expression, sequence_of_monomorphic, trim_whitespace_around, BoxedParser, Expr,
    Identifier, Keywords, LiteralExpr, Number, Parser,
};

use super::{any_of_monomorphic, map, optional, pair, triplet, ParseResult};

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

pub fn parse_identifier_as_identifier<'a>() -> impl Parser<'a, Identifier> {
    parse_identifier().map(|ident| Identifier(ident.to_string()))
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

pub fn parse_print_keyword<'a>() -> impl Parser<'a, Expr> {
    parse_literal("print").map(|print_as_expr| Expr::Literal(LiteralExpr::Keyword(Keywords::Print)))
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
    pub identifier: Identifier, // we only want Identifier type here, however, ..
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfBlock {
    pub cond: Expr,
    pub truthy_statement: Statement,
    pub else_statement: Option<Statement>,
}

// TODO: unify this with other "statement" parser in grammar
pub fn parse_expr<'a>() -> impl Parser<'a, Expr> {
    /*
    either(
        either(parse_unary_expression(), parse_binary_expression()),
        parse_expr_literal(),
    )
    */
    any_of_monomorphic(vec![
        BoxedParser::new(parse_binary_expression()),
        BoxedParser::new(parse_unary_expression),
        BoxedParser::new(parse_expr_literal()),
        BoxedParser::new(parse_grouping_expr_2), // no clue how this works and i am very tired
    ])
}

pub fn parse_let_binding<'a>() -> impl Parser<'a, LetBinding> {
    sequence_of_monomorphic(vec![
        BoxedParser::new(parse_let_keyword()),
        BoxedParser::new(at_least_one_whitespace().map(|_| Expr::Literal(LiteralExpr::Empty))),
        BoxedParser::new(parse_identifier_as_expr()),
        BoxedParser::new(
            trim_whitespace_around(parse_literal("=")).map(|_| Expr::Literal(LiteralExpr::Empty)),
        ),
        BoxedParser::new(parse_expr()),
        BoxedParser::new(parse_literal(";").map(|_| Expr::Literal(LiteralExpr::Empty))),
    ])
    .map(|results| LetBinding {
        identifier: ensure_is_identifier(results.get(2).unwrap().clone()),
        rhs: results.get(4).unwrap().clone(),
    })
}

pub fn parse_if_block_beginning<'a>() -> impl Parser<'a, Expr> {
    sequence_of_monomorphic(vec![
        BoxedParser::new(trim_whitespace_around(parse_literal("if")))
            .map(|_| Expr::Literal(LiteralExpr::Empty)),
        BoxedParser::new(trim_whitespace_around(parse_literal("(")))
            .map(|_| Expr::Literal(LiteralExpr::Empty)),
        BoxedParser::new(trim_whitespace_around(parse_expr())),
        BoxedParser::new(trim_whitespace_around(parse_literal(")")))
            .map(|_| Expr::Literal(LiteralExpr::Empty)),
    ])
    .map(|sequence| sequence.get(2).unwrap().clone())
}

pub fn parse_else_clause<'a>() -> impl Parser<'a, Statement> {
    pair(
        BoxedParser::new(trim_whitespace_around(parse_literal("else")))
            .map(|_| Expr::Literal(LiteralExpr::Empty)),
        BoxedParser::new(parse_statement()),
    )
    .map(|(_, statement)| statement)
}

pub fn parse_if_block<'a>(input: &'a str) -> ParseResult<'a, IfBlock> {
    triplet(
        BoxedParser::new(parse_if_block_beginning()),
        BoxedParser::new(trim_whitespace_around(parse_statement())),
        BoxedParser::new(optional(trim_whitespace_around(parse_else_clause()))),
    )
    .map(|(conditional_expr, statement, else_statement)| IfBlock {
        cond: conditional_expr,
        truthy_statement: statement,
        else_statement,
    })
    .parse(input)
}

pub fn ensure_is_identifier(e: Expr) -> Identifier {
    match e {
        Expr::Literal(LiteralExpr::Identifier(ident)) => ident,
        _ => panic!("Something bad happened"),
    }
}

pub fn parse_print_statement<'a>() -> impl Parser<'a, Expr> {
    sequence_of_monomorphic(vec![
        BoxedParser::new(parse_print_keyword()),
        BoxedParser::new(at_least_one_whitespace().map(|_| Expr::Literal(LiteralExpr::Empty))),
        BoxedParser::new(parse_expr()),
        BoxedParser::new(parse_literal(";").map(|_| Expr::Literal(LiteralExpr::Empty))),
    ])
    .map(|results| results.get(2).unwrap().clone())
}

pub fn parse_grouping_expr<'a>() -> impl Parser<'a, Expr> {
    triplet(
        trim_whitespace_around(parse_literal("(")),
        parse_expr(),
        trim_whitespace_around(parse_literal(")")),
    )
    .map(|(_, expr, _)| Expr::Grouping(Box::new(expr)))
}

/// Poor man's lazy eval :{
/// We can not call parse_expr directly when construction parse_grouping_expr
pub fn parse_grouping_expr_2<'a>(input: &'a str) -> ParseResult<'a, Expr> {
    let (rest, res) = triplet(
        trim_whitespace_around(parse_literal("(")),
        parse_expr(),
        trim_whitespace_around(parse_literal(")")),
    )
    .parse(input)?;

    Ok((rest, Expr::Grouping(Box::new(res.1))))
}

#[cfg(test)]
#[path = "primitives.test.rs"]
mod tests;
