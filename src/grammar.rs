use crate::parser::{
    any_of_monomorphic, either, either_polymorphic, parse_binary_expression, parse_expr_literal,
    parse_let_binding, parse_unary_expression, trim_whitespace_around, zero_or_more, BoxedParser,
    Expr, Identifier, LetBinding, LiteralExpr, Parser,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expr(Expr),
    Print(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Let(LetBinding),
    Statement(Statement),
}

pub fn parse_expr_statement<'a>() -> impl Parser<'a, Expr> {
    any_of_monomorphic(vec![
        BoxedParser::new(parse_binary_expression()),
        BoxedParser::new(parse_unary_expression()),
        BoxedParser::new(parse_expr_literal()),
    ])
}

pub fn parse_statement_as_expr<'a>() -> impl Parser<'a, Statement> {
    // expr | print statement
    parse_expr_statement().map(|expr| Statement::Expr(expr))
}

pub fn parse_declaration<'a>() -> BoxedParser<'a, Declaration> {
    trim_whitespace_around(
        either_polymorphic(
            BoxedParser::new(parse_let_binding()),
            BoxedParser::new(parse_statement_as_expr()),
        )
        .map(|declaration_candidate| match declaration_candidate {
            (Some(let_binding), None) => Declaration::Let(let_binding),
            (None, Some(statement)) => Declaration::Statement(statement),
            _ => todo!("err reporting here"),
        }),
    )
}

pub type Program = Vec<Declaration>;

pub fn parse_program<'a>() -> impl Parser<'a, Program> {
    zero_or_more(parse_declaration())
}

#[cfg(test)]
#[path = "grammar.test.rs"]
mod tests;
