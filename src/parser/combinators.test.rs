use crate::parser::{
    left, one_or_more, pair, parse_literal, parse_number, right, sequence_of_monomorphic,
    zero_or_more, Parser,
};

#[test]
fn test_left() {
    let src = "joe130";
    let parser = left(parse_literal("joe"), parse_number());

    let result = parser.parse(src).unwrap();

    assert_eq!(("", "joe"), result);
}

#[test]
fn test_right() {
    let src = "joe130";
    let parser = right(parse_literal("joe"), parse_number());

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
fn test_pair_parser() {
    let combined_parser = pair(parse_literal("("), parse_number());
    let src = "(130 + 15)";

    let result = combined_parser.parse(src).unwrap();

    assert_eq!((" + 15)", ("(", "130")), result);
}

#[test]
fn test_sequence_of_monomorphic<'a>() {
    let src = "foo123bar.";

    let parsers = parse_literal("foo").and_then(|_| parse_number());
    // let parser = parsers.parse(src)
    // sequence_of_monomorphic .. ;
    let result = parsers.parse(src);

    println!("{:#?}", result);
}
