use crate::grammar::{
    parse_declaration, parse_declaration_without_trailing_semicolon, parse_statement,
    wrapped_scope, Declaration, Statement,
};
use crate::parser::{
    and_then, at_least_one_whitespace, either, optional_whitespace, parse_binary_expression,
    parse_expr_literal, parse_function_call, parse_unary_expression, sequence_of_monomorphic,
    trim_whitespace_around, zero_or_more, BoxedParser, Expr, Identifier, LiteralExpr, Number,
    Parser,
};

use super::{
    any_of_monomorphic, map, optional, pair, parse_function_call_for_expr, triplet, ParseResult,
    StringLiteral,
};

use ordered_float::OrderedFloat;

pub fn parse_number<'a>() -> impl Parser<'a, &'a str> {
    move |input: &'a str| {
        let mut idx: usize = 0;
        let mut has_dot = false;

        while let Some(ch) = input.chars().nth(idx) {
            if ch == '.' {
                if !has_dot {
                    has_dot = true;
                    idx += 1;
                } else {
                    return Err(String::from(
                        "Float64 can not have >= 1 dot in its definition",
                    ));
                }
            } else if ch.is_digit(10) {
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
                let has_dot = m.contains(".");
                let literal_expr = if has_dot {
                    let parsed: Result<f64, String> = m
                        .parse::<f64>()
                        .map_err(|_| String::from("Could not parse to i32"));

                    LiteralExpr::NumberLiteral(Number::Float64(OrderedFloat(parsed?)))
                } else {
                    let parsed: Result<i32, String> = m
                        .parse::<i32>()
                        .map_err(|_| String::from("Could not parse to i32"));

                    LiteralExpr::NumberLiteral(Number::Integer32(parsed?))
                };

                Ok((rest, Expr::Literal(literal_expr)))
            }
            Err(e) => Err(e),
        }
    }
    //.map(|num| Expr::Literal(LiteralExpr::NumberLiteral(Number(num.parse().unwrap()))))
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
pub struct Reassignment {
    pub identifier: Identifier, // we only want Identifier type here, however, ..
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfBlock {
    pub cond: Expr,
    pub truthy_statement: Statement,
    pub else_statement: Option<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoop {
    pub condition: Expr,
    pub body: Statement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForLoop {
    pub init_declaration: Declaration,
    pub condition: Expr,
    pub post_iteration: Declaration,
    pub body: Statement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub identifier: Identifier,             // ?
    pub arguments: Option<Vec<Identifier>>, // ?
    pub body: Statement,
}

// TODO: unify this with other "statement" parser in grammar
pub fn parse_expr<'a>(input: &'a str) -> ParseResult<'a, Expr> {
    any_of_monomorphic(vec![
        BoxedParser::new(parse_binary_expression),
        BoxedParser::new(parse_unary_expression),
        BoxedParser::new(parse_function_call_for_expr),
        BoxedParser::new(parse_expr_literal),
        BoxedParser::new(parse_grouping_expr_2), // no clue how this works and i am very tired
    ])
    .parse(input)
}

pub fn parse_let_binding<'a>() -> impl Parser<'a, LetBinding> {
    sequence_of_monomorphic(vec![
        BoxedParser::new(parse_literal("let")).map(|_| Expr::Literal(LiteralExpr::Empty)),
        BoxedParser::new(at_least_one_whitespace().map(|_| Expr::Literal(LiteralExpr::Empty))),
        BoxedParser::new(parse_identifier_as_expr()),
        BoxedParser::new(
            trim_whitespace_around(parse_literal("=")).map(|_| Expr::Literal(LiteralExpr::Empty)),
        ),
        BoxedParser::new(parse_expr),
        // BoxedParser::new(parse_literal(";").map(|_| Expr::Literal(LiteralExpr::Empty))),
    ])
    .map(|results| LetBinding {
        identifier: ensure_is_identifier(results.get(2).unwrap().clone()),
        rhs: results.get(4).unwrap().clone(),
    })
}

pub fn parse_reassignment<'a>() -> impl Parser<'a, Reassignment> {
    sequence_of_monomorphic(vec![
        BoxedParser::new(trim_whitespace_around(parse_identifier_as_expr())),
        BoxedParser::new(
            trim_whitespace_around(parse_literal("=")).map(|_| Expr::Literal(LiteralExpr::Empty)),
        ),
        BoxedParser::new(parse_expr),
    ])
    .map(|results| Reassignment {
        identifier: ensure_is_identifier(results.get(0).unwrap().clone()),
        rhs: results.get(2).unwrap().clone(),
    })
}

pub fn end_with_semicolon<'a, P, R>(parser: P) -> impl Parser<'a, R>
where
    P: Parser<'a, R> + 'a,
    R: 'a,
{
    pair(parser, trim_whitespace_around(parse_literal(";"))).map(|(r1, _)| r1)
}

pub fn parse_if_block_beginning<'a>() -> impl Parser<'a, Expr> {
    sequence_of_monomorphic(vec![
        BoxedParser::new(trim_whitespace_around(parse_literal("if")))
            .map(|_| Expr::Literal(LiteralExpr::Empty)),
        BoxedParser::new(trim_whitespace_around(parse_literal("(")))
            .map(|_| Expr::Literal(LiteralExpr::Empty)),
        BoxedParser::new(trim_whitespace_around(parse_expr)),
        BoxedParser::new(trim_whitespace_around(parse_literal(")")))
            .map(|_| Expr::Literal(LiteralExpr::Empty)),
    ])
    .map(|sequence| sequence.get(2).unwrap().clone())
}

pub fn parse_else_clause<'a>() -> impl Parser<'a, Statement> {
    pair(
        BoxedParser::new(trim_whitespace_around(parse_literal("else")))
            .map(|_| Expr::Literal(LiteralExpr::Empty)),
        BoxedParser::new(parse_statement),
    )
    .map(|(_, statement)| statement)
}

pub fn parse_if_block<'a>(input: &'a str) -> ParseResult<'a, IfBlock> {
    triplet(
        BoxedParser::new(parse_if_block_beginning()),
        BoxedParser::new(trim_whitespace_around(parse_statement)),
        BoxedParser::new(optional(trim_whitespace_around(parse_else_clause()))),
    )
    .map(|(conditional_expr, statement, else_statement)| IfBlock {
        cond: conditional_expr,
        truthy_statement: statement,
        else_statement,
    })
    .parse(input)
}

pub fn parse_while_loop<'a>(input: &'a str) -> ParseResult<'a, WhileLoop> {
    pair(
        trim_whitespace_around(sequence_of_monomorphic(vec![
            BoxedParser::new(trim_whitespace_around(parse_literal("while")))
                .map(|_| Expr::Literal(LiteralExpr::Empty)),
            BoxedParser::new(trim_whitespace_around(parse_literal("(")))
                .map(|_| Expr::Literal(LiteralExpr::Empty)),
            BoxedParser::new(trim_whitespace_around(parse_expr)),
            BoxedParser::new(trim_whitespace_around(parse_literal(")")))
                .map(|_| Expr::Literal(LiteralExpr::Empty)),
        ])),
        trim_whitespace_around(parse_statement),
    )
    .map(|(conditional_expr, body)| WhileLoop {
        condition: conditional_expr.get(2).unwrap().clone(),
        body,
    })
    .parse(input)
}

// "for" "(" ("declaration | "expr"?) ";" "expr" ";" "expr" ")" statement
// TODO: this is pretty bad, i should rewrite it + rethink how expr / statements / decls are
// handled lol
pub fn parse_for_loop<'a>(input: &'a str) -> ParseResult<'a, ForLoop> {
    let entry = pair(
        BoxedParser::new(trim_whitespace_around(parse_literal("for")))
            .map(|_| Expr::Literal(LiteralExpr::Empty)),
        BoxedParser::new(trim_whitespace_around(parse_literal("(")))
            .map(|_| Expr::Literal(LiteralExpr::Empty)),
    );

    // parse_declaration
    let mid = pair(
        BoxedParser::new(trim_whitespace_around(parse_expr)),
        BoxedParser::new(trim_whitespace_around(parse_literal(";")))
            .map(|_| Expr::Literal(LiteralExpr::Empty)),
    );

    let last = pair(
        BoxedParser::new(trim_whitespace_around(
            parse_declaration_without_trailing_semicolon,
        )),
        BoxedParser::new(trim_whitespace_around(parse_literal(")"))),
    );

    let for_loop_definition_body = triplet(
        entry,
        BoxedParser::new(trim_whitespace_around(parse_declaration)),
        pair(mid, last),
    );

    pair(
        for_loop_definition_body,
        trim_whitespace_around(parse_statement),
    )
    .map(
        |((entry_pair, decl, (mid_pair, last_pair)), body)| ForLoop {
            init_declaration: decl,
            condition: mid_pair.0,
            post_iteration: last_pair.0,
            body,
        },
    )
    .parse(input)
}

pub fn ensure_is_identifier(e: Expr) -> Identifier {
    match e {
        Expr::Literal(LiteralExpr::Identifier(ident)) => ident,
        _ => panic!("Something bad happened"),
    }
}

pub fn parse_print_statement<'a>(input: &'a str) -> ParseResult<'a, Expr> {
    sequence_of_monomorphic(vec![
        BoxedParser::new(parse_literal("print")).map(|_| Expr::Literal(LiteralExpr::Empty)),
        BoxedParser::new(at_least_one_whitespace().map(|_| Expr::Literal(LiteralExpr::Empty))),
        BoxedParser::new(parse_expr),
        BoxedParser::new(parse_literal(";").map(|_| Expr::Literal(LiteralExpr::Empty))),
    ])
    .map(|results| results.get(2).unwrap().clone())
    .parse(input)
}

pub fn parse_grouping_expr<'a>() -> impl Parser<'a, Expr> {
    triplet(
        trim_whitespace_around(parse_literal("(")),
        parse_expr,
        trim_whitespace_around(parse_literal(")")),
    )
    .map(|(_, expr, _)| Expr::Grouping(Box::new(expr)))
}

/// Poor man's lazy eval :{
/// We can not call parse_expr directly when construction parse_grouping_expr
pub fn parse_grouping_expr_2<'a>(input: &'a str) -> ParseResult<'a, Expr> {
    let (rest, res) = triplet(
        trim_whitespace_around(parse_literal("(")),
        parse_expr,
        trim_whitespace_around(parse_literal(")")),
    )
    .parse(input)?;

    Ok((rest, Expr::Grouping(Box::new(res.1))))
}

pub fn parse_string_literal<'a>() -> impl Parser<'a, Expr> {
    move |input: &'a str| {
        let p = trim_whitespace_around(parse_literal("\""));
        let (rest, matched) = p.parse(input)?;

        let mut len: usize = 0;
        let mut chars = rest.chars();

        loop {
            match chars.next() {
                Some(ch) => {
                    if ch != '"' {
                        len += 1;
                    } else {
                        break;
                    }
                }
                None => return Err(String::from("UnexpectedEOF [parse_string_literal]")),
            }
        }

        let string = &input[1..len + 1];
        let rest = &input[len + 2..];
        Ok((
            rest,
            Expr::Literal(LiteralExpr::StringLiteral(StringLiteral(
                string.to_string(),
            ))),
        ))
    }
}

/// from a caller's perspective it's okay to take Exprs
/// from a definition perspective, it'd be ideal to take only Identifier, but we can live with this
/// for now
pub fn parse_arguments<'a>(input: &'a str) -> ParseResult<'a, Vec<Expr>> {
    pair(
        parse_expr,
        optional(zero_or_more(
            pair(trim_whitespace_around(parse_literal(",")), parse_expr).map(|(_, expr)| expr),
        )),
    )
    .map(|(first, maybe_rest)| {
        let mut v = Vec::new();
        v.push(first);

        if let Some(mut rest) = maybe_rest {
            v.append(&mut rest);
        }

        v
    })
    .parse(input)
}
// Expr needs to evaluate into something callable
pub fn parse_function_definition<'a>(input: &'a str) -> ParseResult<'a, FnDef> {
    let arguments_parser = optional(parse_arguments);

    triplet(
        sequence_of_monomorphic(vec![
            BoxedParser::new(trim_whitespace_around(parse_literal("fun"))),
            BoxedParser::new(trim_whitespace_around(parse_identifier())),
            BoxedParser::new(trim_whitespace_around(parse_literal("("))),
        ])
        .map(|sequence| Identifier(sequence.get(1).unwrap().to_string())),
        pair(
            BoxedParser::new(trim_whitespace_around(arguments_parser)),
            BoxedParser::new(trim_whitespace_around(parse_literal(")"))),
        ),
        BoxedParser::new(trim_whitespace_around(parse_statement)), // fn def
    )
    .map(|(identifier, (maybe_arguments, _), body)| FnDef {
        identifier,
        arguments: maybe_arguments.map(|arguments| {
            arguments
                .iter()
                .map(|expr| ensure_is_identifier(expr.clone()))
                .collect()
        }),
        body,
    })
    .parse(input)
}

pub fn parse_return_statement<'a>(input: &'a str) -> ParseResult<'a, Option<Expr>> {
    end_with_semicolon(pair(
        trim_whitespace_around(parse_literal("return")),
        optional(parse_expr),
    ))
    .map(|(_, maybe_expr)| maybe_expr)
    .parse(input)
}

#[cfg(test)]
#[path = "primitives.test.rs"]
mod tests;
