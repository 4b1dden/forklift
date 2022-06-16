use std::fmt::Debug;

use crate::parser::{map, BoxedParser, ParseResult, Parser};

pub fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input),
    }
}

pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
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

pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left_result, _)| left_result)
}

pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_, right_result)| right_result)
}

pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
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

pub fn any_of_monomorphic<'a, P, R>(parsers: Vec<P>) -> impl Parser<'a, R>
where
    P: Parser<'a, R>,
{
    move |input| {
        let mut results = Vec::<R>::new();
        let mut rest = input;

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

pub fn sequence_of_monomorphic<'a, R>(parsers: Vec<BoxedParser<'a, R>>) -> impl Parser<'a, Vec<R>>
where
    R: Debug,
{
    move |input| {
        let mut results = Vec::<R>::new();
        let mut rest = input;

        for parser in parsers.iter() {
            let res = parser.parse(rest);
            if res.is_err() {
                return Err(String::from("Unexpected sequence"));
            } else {
                let (remaining, matched) = res.unwrap();
                rest = remaining;
                results.push(matched);
            }
        }

        Ok((rest, results))
    }
}

pub fn triplet<'a, P1, P2, P3, R1, R2, R3>(
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

pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
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

#[cfg(test)]
#[path = "combinators.test.rs"]
mod tests;
