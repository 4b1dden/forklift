use crate::parser::{
    any_of_monomorphic, either, either_polymorphic, parse_binary_expression, parse_expr_literal,
    parse_let_binding, BoxedParser, Expr, Identifier, LetBinding, LiteralExpr, Parser,
};

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    Print(Expr),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Let(LetBinding),
    Statement(Statement),
}

pub fn parse_expr_statement<'a>() -> impl Parser<'a, Expr> {
    any_of_monomorphic(vec![
        BoxedParser::new(parse_binary_expression()),
        BoxedParser::new(parse_expr_literal()),
    ])
}

pub fn parse_statement_as_expr<'a>() -> impl Parser<'a, Statement> {
    // expr | print statement
    parse_expr_statement().map(|expr| Statement::Expr(expr))
}

pub fn parse_declaration<'a>() -> BoxedParser<'a, Declaration> {
    either_polymorphic(
        BoxedParser::new(parse_let_binding()),
        BoxedParser::new(parse_statement_as_expr()),
    )
    .map(|declaration_candidate| match declaration_candidate {
        (Some(let_binding), None) => Declaration::Let(let_binding),
        (None, Some(statement)) => Declaration::Statement(statement),
        _ => todo!("err reporting here"),
    })
}

pub type Program = Vec<Declaration>;
