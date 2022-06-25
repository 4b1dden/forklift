use crate::parser::{
    any_of_monomorphic, either, either_polymorphic, parse_binary_expression, parse_expr_literal,
    parse_let_binding, parse_literal, parse_print_statement, parse_unary_expression,
    trim_whitespace_around, triplet, zero_or_more, BoxedParser, Expr, Identifier, LetBinding,
    LiteralExpr, ParseResult, ParseResult, Parser,
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
    ScopedBlock(Vec<Declaration>),
}

pub fn parse_expr_statement<'a>() -> impl Parser<'a, Expr> {
    any_of_monomorphic(vec![
        BoxedParser::new(parse_binary_expression()),
        BoxedParser::new(parse_unary_expression),
        BoxedParser::new(parse_expr_literal()),
    ])
}

pub fn parse_statement_as_expr<'a>() -> impl Parser<'a, Statement> {
    // expr | print statement
    any_of_monomorphic(vec![
        BoxedParser::new(parse_print_statement().map(|expr| Statement::Print(expr))),
        BoxedParser::new(parse_expr_statement().map(|expr| Statement::Expr(expr))),
    ])
}

pub fn parse_declaration<'a>(input: &'a str) -> ParseResult<'a, Declaration> {
    trim_whitespace_around(any_of_monomorphic(vec![
        BoxedParser::new(parse_let_binding()).map(|let_binding| Declaration::Let(let_binding)),
        BoxedParser::new(parse_statement_as_expr())
            .map(|statement| Declaration::Statement(statement)),
        BoxedParser::new(wrapped_scope(zero_or_more(parse_declaration)))
            .map(|block| Declaration::ScopedBlock(block)),
    ]))
    .parse(input)
}

pub type Program = Vec<Declaration>;

pub fn wrapped_scope<'a, P, R>(inside: P) -> impl Parser<'a, R>
where
    P: Parser<'a, R> + 'a,
    R: 'a,
{
    triplet(parse_literal("{"), inside, parse_literal("}"))
        .map(|(_, inside_result, _)| inside_result)
}

pub fn parse_program<'a>() -> impl Parser<'a, Program> {
    zero_or_more(either(
        wrapped_scope(zero_or_more(parse_declaration()))
            .map(|declarations| Declaration::ScopedBlock(declarations)),
        parse_declaration(),
    ))
}

#[cfg(test)]
#[path = "grammar.test.rs"]
mod tests;
