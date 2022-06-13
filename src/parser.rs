use std::fmt::Debug;

#[derive(Debug)]
pub enum GrammarItem {
    NumberLiteral(i32),
}

type ParseResult<'a, T> = Result<(&'a str, T), String>;

fn parse_number(input: &str) -> ParseResult<&str> {
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

fn parse_identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    if let Some(next) = chars.next() {
        if next.is_alphabetic() || next == '_' {
            matched.push(next);
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
            matched.push(next);
        } else {
            break;
        }
    }

    Ok((&input[matched.len()..], matched))
}

trait Parser<'a, Output> {
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

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(LiteralExpr),
    Binary(BinaryExpr),
}

#[derive(Clone, Debug)]
pub struct LiteralExpr(i32);

#[derive(Clone, Debug)]
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

fn parse_literal<'a>(literal: &'a str) -> impl Parser<'a, &str> {
    move |input: &'a str| match input.get(0..literal.len()) {
        Some(substr) => {
            if substr == literal {
                Ok((&input[substr.len()..], substr))
            } else {
                Err(format!("expected {}, got {}", literal.to_string(), substr))
            }
        }
        None => Err("Unexpected EOF".to_string()),
    }
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(rest, result1)| {
            parser2
                .parse(rest)
                .map(|(rest2, result2)| (rest2, (result1, result2)))
        })
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
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

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left_result, _)| left_result)
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((rest, item)) = parser.parse(input) {
            input = rest;
            result.push(item);
        } else {
            return Err(String::from("We needed one_or_more, got zero"));
        }

        while let Ok((rest, item)) = parser.parse(input) {
            input = rest;
            result.push(item);
        }

        Ok((input, result))
    }
}

fn sequence_of() {
    todo!()
}

fn any_of_monomorphic<'a, P, R>(parsers: Vec<P>) -> impl Parser<'a, R>
where
    P: Parser<'a, R>,
{
    move |input| {
        for parser in parsers.iter() {
            let result = parser.parse(input);
            if let (Ok(_)) = result {
                return result;
            }
        }

        Err(String::from(
            "none of the provided parsers matched in any_of_monomorphic",
        ))
    }
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

fn triplet<'a, P1, P2, P3, R1, R2, R3>(
    parser1: P1,
    parser2: P2,
    parser3: P3,
) -> impl Parser<'a, (R1, R2, R3)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
    P3: Parser<'a, R3>,
{
    move |input| {
        parser1.parse(input).and_then(|(rest1, result1)| {
            parser2.parse(rest1).and_then(|(rest2, result2)| {
                parser3
                    .parse(rest2)
                    .map(|(rest3, result3)| (rest3, (result1, result2, result3)))
            })
        })
    }
}

fn next_char(input: &str) -> ParseResult<char> {
    input
        .chars()
        .next()
        .map(|ch| (&input[ch.len_utf8()..], ch))
        .ok_or(String::from("Unexpected EOF"))
}

fn predicate<'a, P, F, A>(parser: P, pred_fn: F) -> impl Parser<'a, A>
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

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((rest, item)) = parser.parse(input) {
            input = rest;
            result.push(item);
        }

        Ok((input, result))
    }
}

fn parse_whitespace<'a>() -> impl Parser<'a, char> {
    predicate(next_char, |c| c.is_whitespace()) // is_whitespace also matches "\n", "\t", etc...
}

fn at_least_one_whitespace<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(parse_whitespace())
}

fn optional_whitespace<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(parse_whitespace())
}

fn trim_whitespace_around<'a, P, R>(parser: P) -> impl Parser<'a, R>
where
    P: Parser<'a, R> + 'a,
    R: 'a,
{
    triplet(optional_whitespace(), parser, optional_whitespace())
        .map(|(_, inside_result, _)| inside_result)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_, right_result)| right_result)
}

// Todo: add parentheses
fn parse_binary_expression<'a>() -> impl Parser<'a, (&'a str, Vec<(BinaryOperator, &'a str)>)> {
    let lhs_number = trim_whitespace_around(parse_number);
    let bin_op = trim_whitespace_around(bin_operand()).map(|op| BinaryOperator::from(op));
    let rhs_number = trim_whitespace_around(parse_number);

    let rhs_parser = pair(bin_op, rhs_number);

    return pair(lhs_number, one_or_more(rhs_parser));
}

// TODO: get rid of this
#[derive(Debug)]
pub enum BinExpInfix {
    Literal(LiteralExpr),
    Op(BinaryOperator),
}

#[derive(Debug)]
pub struct ASTNode {
    pub left: Option<Box<ASTNode>>,
    pub right: Option<Box<ASTNode>>,
    pub val: BinExpInfix,
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
fn infix_to_ast(infix: Vec<BinExpInfix>) -> ASTNode {
    let mut operator_stack = vec![];
    let mut operand_stack: Vec<ASTNode> = vec![];

    for infix_item in infix.iter() {
        match infix_item {
            BinExpInfix::Literal(literal) => {
                operand_stack.push(ASTNode {
                    left: None,
                    right: None,
                    val: BinExpInfix::Literal(literal.clone()),
                });
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

fn add_node(stack: &mut Vec<ASTNode>, op: BinaryOperator) {
    let right = stack.pop().unwrap();
    let left = stack.pop().unwrap();
    stack.push(ASTNode {
        left: Some(Box::new(left)),
        right: Some(Box::new(right)),
        val: BinExpInfix::Op(op),
    });
}

// Todo: get rid of this completely
pub fn parse_binary_to_infix<'a>(
    result: (&'a str, (&'a str, Vec<(BinaryOperator, &'a str)>)),
) -> Vec<BinExpInfix> {
    let mut infix = vec![];

    infix.push(BinExpInfix::Literal(LiteralExpr(
        result.1 .0.parse().unwrap(),
    )));

    for item in result.1 .1.iter() {
        infix.push(BinExpInfix::Op(item.0.clone()));
        infix.push(BinExpInfix::Literal(LiteralExpr(item.1.parse().unwrap())));
    }

    infix
}

#[cfg(test)]
mod tests {
    use super::{
        any_of_monomorphic, at_least_one_whitespace, bin_operand, infix_to_ast, left, map,
        one_or_more, optional_whitespace, pair, parse_binary_expression, parse_binary_to_infix,
        parse_identifier, parse_literal, parse_number, predicate, right, trim_whitespace_around,
        triplet, zero_or_more, BinExpInfix, BinaryExpr, BinaryOperator, Expr, GrammarItem,
        LiteralExpr, Parser,
    };

    #[test]
    fn test_parse_number() {
        let src = "12 foo";
        let result = parse_number(src).unwrap();

        assert_eq!((" foo", "12"), result);
    }

    #[test]
    fn test_parse_identifier() {
        let src = "foo_bar123";
        let result = parse_identifier(src).unwrap();

        assert_eq!(("", String::from("foo_bar123")), result);

        assert!(parse_identifier("123foo_bar").is_err());
        assert!(parse_identifier("_foo_bar123").is_ok());
    }

    #[test]
    fn test_parse_lparen() {
        let lparen_parser = parse_literal("(");

        let src = "(12 + 3)";
        let result = lparen_parser.parse(src).unwrap();

        assert_eq!(("12 + 3)", "("), result);
    }

    #[test]
    fn test_pair_parser() {
        let combined_parser = pair(parse_literal("("), parse_number);
        let src = "(130 + 15)";

        let result = combined_parser.parse(src).unwrap();

        assert_eq!((" + 15)", ("(", "130")), result);
    }

    #[test]
    fn test_map() {
        fn mapper(src: &str) -> usize {
            src.len()
        }

        let src = "130 150";
        let combined_parser = map(parse_number, mapper);

        let result = combined_parser.parse(src).unwrap();

        assert_eq!((" 150", 3), result);
    }

    #[test]
    fn test_left() {
        let src = "joe130";
        let parser = left(parse_literal("joe"), parse_number);

        let result = parser.parse(src).unwrap();

        assert_eq!(("", "joe"), result);
    }

    #[test]
    fn test_right() {
        let src = "joe130";
        let parser = right(parse_literal("joe"), parse_number);

        let result = parser.parse(src).unwrap();

        assert_eq!(("", "130"), result);
    }

    #[test]
    fn test_one_or_more() {
        let src = "aaaaahello";
        let parser = one_or_more(parse_literal("a"));

        let result = parser.parse(src).unwrap();

        assert_eq!(("hello", vec!["a", "a", "a", "a", "a"]), result);
    }

    #[test]
    fn test_one_or_more_fails() {
        let src = "hello";
        let parser = one_or_more(parse_literal("a"));

        let result = parser.parse(src);

        assert!(result.is_err()); // TODO: extend to precise error variant check
    }

    #[test]
    fn test_zero_or_more() {
        let src = "aahello";
        let a_parser = zero_or_more(parse_literal("a"));
        let b_parser = zero_or_more(parse_literal("b"));

        let result_a = a_parser.parse(src).unwrap();
        let result_b = b_parser.parse(src).unwrap();

        assert_eq!(("hello", vec!["a", "a"]), result_a);
        assert_eq!(("aahello", vec![]), result_b);
    }

    #[test]
    fn test_any_char_and_pred() {
        let src = "1234 foo";
        let four_digit_num = predicate(parse_number, |num| num.len() == 4);

        let result = four_digit_num.parse(src);

        assert_eq!(Ok((" foo", "1234")), result);

        assert!(four_digit_num.parse("123 foo").is_err());
    }

    #[test]
    fn test_at_least_one_whitespace() {
        let src = "   foo = bar";
        let parser = at_least_one_whitespace();

        let result = parser.parse(src).unwrap();

        assert_eq!(("foo = bar", vec![' ', ' ', ' ']), result);

        let src2 = "foo = bar";
        assert!(parser.parse(src2).is_err());
    }

    const BINARY_EXPR_DEFAULT: &str = "   3 * 100    + 4 * 2";
    #[test]
    fn test_parse_binary_expression() {
        let parser = parse_binary_expression();

        let result = parser.parse(BINARY_EXPR_DEFAULT).unwrap();

        assert_eq!(
            (
                "",
                (
                    "3",
                    vec![
                        (BinaryOperator::Mul, "100"),
                        (BinaryOperator::Plus, "4"),
                        (BinaryOperator::Mul, "2")
                    ]
                )
            ),
            result
        );
    }

    #[test]
    fn test_binary_expression_to_ast() {
        let parser = parse_binary_expression();
        let parsed = parser.parse(BINARY_EXPR_DEFAULT).unwrap();
        let infix = parse_binary_to_infix(parsed);

        let ast = infix_to_ast(infix);

        println!("{:#?}", ast);
        // TODO: write test case once ASTNode type semantics are stabilised
    }

    #[test]
    fn test_any_of_monomorphic() {
        let combined_any_parser = any_of_monomorphic(vec![
            parse_literal("1"),
            parse_literal("2"),
            parse_literal("3"),
        ]);

        assert_eq!(("foo", "1"), combined_any_parser.parse("1foo").unwrap());
        assert_eq!(("foo", "2"), combined_any_parser.parse("2foo").unwrap());
        assert_eq!(("foo", "3"), combined_any_parser.parse("3foo").unwrap());

        assert!(combined_any_parser.parse("4foo").is_err());
    }
}
