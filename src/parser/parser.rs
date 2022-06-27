use std::fmt::Debug;

use crate::parser::{
    any_of_monomorphic, either, left, one_or_more, pair, parse_grouping_expr_2, parse_identifier,
    parse_literal, parse_number, parse_string_literal, right, triplet, zero_or_more,
};

use super::{parse_identifier_as_expr, parse_number_as_expr};

pub type ParseResult<'a, T> = Result<(&'a str, T), String>;

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
    // Arithmetic
    Plus,
    Minus,
    Div,
    Mul,

    //Logical
    EqEq,
    BangEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

impl From<&str> for BinaryOperator {
    fn from(incoming: &str) -> Self {
        match incoming {
            "+" => BinaryOperator::Plus,
            "-" => BinaryOperator::Minus,
            "/" => BinaryOperator::Div,
            "*" => BinaryOperator::Mul,
            "==" => BinaryOperator::EqEq,
            "!=" => BinaryOperator::BangEq,
            "<=" => BinaryOperator::LessEq,
            "<" => BinaryOperator::Less,
            ">" => BinaryOperator::Greater,
            ">=" => BinaryOperator::GreaterEq,
            _ => panic!("unknown bin operator"),
        }
    }
}

impl From<&str> for UnaryOperator {
    fn from(incoming: &str) -> Self {
        match incoming {
            "-" => UnaryOperator::Minus,
            "!" => UnaryOperator::Bang,
            _ => panic!("not implemented"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Grouping(Box<Expr>),
}

pub fn parse_expr_literal<'a>() -> impl Parser<'a, Expr> {
    any_of_monomorphic(vec![
        BoxedParser::new(parse_identifier_as_expr()),
        BoxedParser::new(parse_number_as_expr()),
        BoxedParser::new(parse_string_literal()),
    ])
}

pub fn parse_single_statement<'a>() -> impl Parser<'a, Expr> {
    // todo: impl fully
    // expr_literal | ... ?
    any_of_monomorphic(vec![
        BoxedParser::new(parse_grouping_expr_2),
        BoxedParser::new(parse_unary_expression),
        BoxedParser::new(parse_expr_literal()),
        BoxedParser::new(parse_string_literal()),
    ])
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralExpr {
    Identifier(Identifier),
    StringLiteral(StringLiteral),
    Keyword(Keywords),
    NumberLiteral(Number),
    // TODO: this is not good for sure
    Empty,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Keywords {
    Let,
    Print, // for early development only, we'll remove this later
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub String);

#[derive(Clone, Debug, PartialEq)]
pub struct StringLiteral(pub String);

#[derive(Clone, Debug, PartialEq)]
pub struct Number(pub i32);

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: BinaryOperator,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOperator,
    pub expr: Box<Expr>,
}

pub fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
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

/*
pub fn and_then_2<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => match f(result) {
            Ok(next_parser) => next_parser
            Err(e) => Err(e),
        },
        Err(e) => Err(e),
    }
}
*/
pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    pub fn new<P>(parser: P) -> Self
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
    let operands = vec!["+", "-", "/", "*", "==", "!=", "<", "<=", ">", ">="];
    any_of_monomorphic(
        operands
            .iter()
            .map(|op_str| parse_literal(op_str.clone()))
            .collect(),
    )
}

fn unary_operand<'a>() -> impl Parser<'a, &'a str> {
    let operands = vec!["-", "!"];
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
// for early development only, we'll remove this later
pub fn parse_whitespace<'a>() -> impl Parser<'a, ()> {
    predicate(next_char, |c| c.is_whitespace()).map(|_| ()) // is_whitespace also matches "\n", "\t", etc...
}

pub fn at_least_one_whitespace<'a>() -> impl Parser<'a, ()> {
    one_or_more(parse_whitespace()).map(|_| ())
}

pub fn optional_whitespace<'a>() -> impl Parser<'a, ()> {
    zero_or_more(parse_whitespace()).map(|_| ())
}

pub fn trim_whitespace_around<'a, P, R>(parser: P) -> BoxedParser<'a, R>
where
    P: Parser<'a, R> + 'a,
    R: 'a,
{
    BoxedParser::new(
        triplet(optional_whitespace(), parser, optional_whitespace())
            .map(|(_, inside_result, _)| inside_result),
    )
}

// Todo: add parentheses
pub fn parse_binary_expression<'a>() -> impl Parser<'a, Expr> {
    let lhs_number = trim_whitespace_around(parse_single_statement());
    let bin_op = trim_whitespace_around(bin_operand()).map(|op| BinaryOperator::from(op));
    let rhs_number = trim_whitespace_around(parse_single_statement());

    let rhs_parser = pair(bin_op, rhs_number);

    return pair(lhs_number, one_or_more(rhs_parser))
        .map(|(base_expr, tuples)| flatten_bin_expr_to_infix(base_expr, tuples))
        .map(|infix_exprs| fold_infix_binary_to_single_expr(infix_exprs));
}

pub fn parse_unary_expression<'a>(input: &'a str) -> ParseResult<'a, Expr> {
    let unary_op = optional_whitespace()
        .and_then(|_| unary_operand().map(|op_char| UnaryOperator::from(op_char)));
    let expr_parser = parse_single_statement();

    pair(unary_op, expr_parser)
        .map(|(unary_op, expr)| {
            Expr::Unary(UnaryExpr {
                op: unary_op,
                expr: Box::new(expr),
            })
        })
        .parse(input)
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
        BinaryOperator::EqEq | BinaryOperator::BangEq => 1,
        BinaryOperator::Less
        | BinaryOperator::LessEq
        | BinaryOperator::Greater
        | BinaryOperator::GreaterEq => 2,
        BinaryOperator::Plus | BinaryOperator::Minus => 3,
        BinaryOperator::Mul | BinaryOperator::Div => 4,
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
