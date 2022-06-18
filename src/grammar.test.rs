use super::parse_declaration;

use crate::parser::{zero_or_more, Parser};
#[test]
fn test_parse_declaration() {
    let parser = parse_declaration();
    let multi = zero_or_more(parse_declaration());

    println!("{:#?}", multi.parse("let b = 3;"));
}
