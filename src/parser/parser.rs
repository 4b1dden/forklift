use std::fmt::Debug;

use crate::parser::{
    any_of_monomorphic, either, left, one_or_more, pair, parse_identifier, parse_literal,
    parse_number, right, triplet, zero_or_more,
};

type ParseResult<'a, T> = Result<(&'a str, T), String>;

pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn map<F, MOut>(self, map_fn: F) -> BoxedParser<'a, MOut>
    where
        Self: Sized + 'a,
        Output: 'a,
        MOut: 'a,
        F: Fn(Output) -> MOut + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn predicate<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a + Debug,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(predicate(self, pred_fn))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Div,
    Mul,
}

impl From<&str> for BinaryOperator {
    fn from(incoming: &str) -> Self {
        match incoming {
            "+" => BinaryOperator::Plus,
            "-" => BinaryOperator::Minus,
            "/" => BinaryOperator::Div,
            "*" => BinaryOperator::Mul,
            _ => panic!("not implemented"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(LiteralExpr),
    Binary(BinaryExpr),
}

pub fn parse_expr_literal<'a>() -> impl Parser<'a, Expr> {
    let identifier_parser = parse_identifier()
        .map(|ident| Expr::Literal(LiteralExpr::StringLiteral(ident.to_string())));
    let number_parser =
        parse_number().map(|num| Expr::Literal(LiteralExpr::NumberLiteral(num.parse().unwrap())));

    either(identifier_parser, number_parser)
}

pub fn parse_expr<'a>() -> impl Parser<'a, Expr> {
    // todo: impl fully
    parse_expr_literal()
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralExpr {
    StringLiteral(String),
    NumberLiteral(i32),
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: BinaryOperator,
    pub rhs: Box<Expr>,
}

fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}

pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn sequence_of() {
    todo!()
}

fn bin_operand<'a>() -> impl Parser<'a, &'a str> {
    let operands = vec!["+", "-", "/", "*"];
    any_of_monomorphic(
        operands
            .iter()
            .map(|op_str| parse_literal(op_str.clone()))
            .collect(),
    )
}

fn next_char(input: &str) -> ParseResult<char> {
    input
        .chars()
        .next()
        .map(|ch| (&input[ch.len_utf8()..], ch))
        .ok_or(String::from("Unexpected EOF"))
}

pub fn predicate<'a, P, F, A>(parser: P, pred_fn: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
    A: Debug,
{
    move |input| {
        parser.parse(input).and_then(|(input_rest, matched)| {
            if pred_fn(&matched) {
                Ok((input_rest, matched))
            } else {
                Err(format!(
                    "got {:?} which did not match inside pred_fn",
                    matched
                ))
            }
        })
    }
}

pub fn parse_whitespace<'a>() -> impl Parser<'a, char> {
    predicate(next_char, |c| c.is_whitespace()) // is_whitespace also matches "\n", "\t", etc...
}

pub fn at_least_one_whitespace<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(parse_whitespace())
}

pub fn optional_whitespace<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(parse_whitespace())
}

pub fn trim_whitespace_around<'a, P, R>(parser: P) -> impl Parser<'a, R>
where
    P: Parser<'a, R> + 'a,
    R: 'a,
{
    triplet(optional_whitespace(), parser, optional_whitespace())
        .map(|(_, inside_result, _)| inside_result)
}

// Todo: add parentheses
pub fn parse_binary_expression<'a>() -> impl Parser<'a, Expr> {
    let lhs_number = trim_whitespace_around(parse_expr());
    let bin_op = trim_whitespace_around(bin_operand()).map(|op| BinaryOperator::from(op));
    let rhs_number = trim_whitespace_around(parse_expr());

    let rhs_parser = pair(bin_op, rhs_number);

    return pair(lhs_number, one_or_more(rhs_parser))
        .map(|(base_expr, tuples)| flatten_bin_expr_to_infix(base_expr, tuples))
        .map(|infix_exprs| fold_infix_binary_to_single_expr(infix_exprs));
}

fn flatten_bin_expr_to_infix(
    base_expr: Expr,
    tuples: Vec<(BinaryOperator, Expr)>,
) -> Vec<BinExpInfix> {
    let mut result = vec![];

    result.push(BinExpInfix::Expr(base_expr));

    for (binary_operator, expr) in tuples.into_iter() {
        result.push(BinExpInfix::Op(binary_operator));
        result.push(BinExpInfix::Expr(expr));
    }

    result
}

// TODO: get rid of this
#[derive(Debug)]
pub enum BinExpInfix {
    Expr(Expr),
    Op(BinaryOperator),
}

fn get_single_operator_precedence(op: &BinaryOperator) -> i32 {
    match op {
        BinaryOperator::Plus | BinaryOperator::Minus => 1,
        BinaryOperator::Mul | BinaryOperator::Div => 2,
    }
}

/// 1 is op2, -1 is op1
fn compare_operator_precedence(op1: &BinaryOperator, op2: &BinaryOperator) -> i32 {
    if get_single_operator_precedence(op2) > get_single_operator_precedence(op1) {
        1
    } else {
        -1
    }
}

/// Shunting yard algorithm
/// https://en.wikipedia.org/wiki/Shunting_yard_algorithm
/// thank mr dijsktra
/// https://www.klittlepage.com/2013/12/22/twelve-days-2013-shunting-yard-algorithm/
pub fn fold_infix_binary_to_single_expr(infix: Vec<BinExpInfix>) -> Expr {
    let mut operator_stack = vec![];
    let mut operand_stack: Vec<Expr> = vec![];

    for infix_item in infix.iter() {
        match infix_item {
            BinExpInfix::Expr(literal) => {
                operand_stack.push(literal.clone());
            }
            BinExpInfix::Op(operator1) => {
                while let Some(top) = operator_stack.last() {
                    if compare_operator_precedence(operator1, top) == 1 {
                        if let Some(popped) = operator_stack.pop() {
                            add_node(&mut operand_stack, popped);
                        }
                    } else {
                        break;
                    }
                }

                operator_stack.push(operator1.clone());
            }
        };
    }

    while !operator_stack.is_empty() {
        add_node(&mut operand_stack, operator_stack.pop().unwrap());
    }

    operand_stack.pop().unwrap()
}

fn add_node(stack: &mut Vec<Expr>, op: BinaryOperator) {
    let right = stack.pop().unwrap();
    let left = stack.pop().unwrap();
    stack.push(Expr::Binary(BinaryExpr {
        lhs: Box::new(left),
        op,
        rhs: Box::new(right),
    }));
}

#[cfg(test)]
#[path = "parser.test.rs"]
mod tests;
