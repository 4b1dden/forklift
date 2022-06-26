use crate::parser::{
    either, left, one_or_more, optional, pair, parse_literal, parse_number, right,
    sequence_of_monomorphic, zero_or_more, BoxedParser, Parser,
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
    let combined = sequence_of_monomorphic(vec![
        BoxedParser::new(parse_literal("foo")),
        BoxedParser::new(parse_number()),
        BoxedParser::new(parse_literal("bar")),
        BoxedParser::new(either(parse_literal("."), parse_literal("!"))),
    ]);

    assert_eq!(
        combined.parse("foo123bar.").unwrap(),
        ("", vec!["foo", "123", "bar", ".",])
    );
    assert_eq!(
        combined.parse("foo123bar!").unwrap(),
        ("", vec!["foo", "123", "bar", "!",])
    );

    assert!(combined.parse("foo123bar_").is_err());
}

#[test]
fn test_optional() {
    let parser = optional(parse_literal("foo"));

    assert_eq!(Ok(("123", None)), parser.parse("123"));
    assert_eq!(Ok(("123", Some("foo"))), parser.parse("foo123"));
}

#[test]
fn test_optional_in_sequence() {
    let parser = sequence_of_monomorphic(vec![
        BoxedParser::new(parse_literal("foo").map(Option::Some)),
        BoxedParser::new(optional(parse_literal("_"))),
        BoxedParser::new(parse_literal("bar").map(Option::Some)),
    ]);

    assert_eq!(
        Ok(("..", vec![Some("foo"), None, Some("bar")])),
        parser.parse("foobar..")
    );
    assert_eq!(
        Ok(("..", vec![Some("foo"), Some("_"), Some("bar")])),
        parser.parse("foo_bar..")
    );

    assert_eq!(
        Err(String::from("expected foo, got wro")),
        parser.parse("wrong start in non-optional")
    );
}
