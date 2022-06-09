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

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
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

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_, right_result)| right_result)
}

#[cfg(test)]
mod tests {
    use super::{
        at_least_one_whitespace, left, map, one_or_more, optional_whitespace, pair, parse_literal,
        parse_number, predicate, right, zero_or_more, GrammarItem, Parser,
    };

    #[test]
    fn test_parse_number() {
        let src = "12 foo";
        let result = parse_number(src).unwrap();

        assert_eq!((" foo", "12"), result);
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

    #[test]
    fn test_plus_seq() {
        // todo:compose properly
        fn plus_seq<'a>() -> impl Parser<'a, String> {
            let lhs_number = left(
                right(optional_whitespace(), parse_number),
                optional_whitespace(),
            );
            let bin_op = left(parse_literal("+"), optional_whitespace());
            return map(
                pair(
                    map(pair(lhs_number, bin_op), |(num, op)| {
                        format!("{} {}", num, op)
                    }),
                    parse_number,
                ),
                |(acc, new_num)| format!("{} {}", acc, new_num),
            );
            //left(lhs_number, bin_op)
        }

        let parser = plus_seq();

        let src = "1 +   2";

        let result = parser.parse(src);
        println!("{:#?}", result);
    }
}
