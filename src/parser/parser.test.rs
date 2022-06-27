use crate::parser::{
    any_of_monomorphic, at_least_one_whitespace, fold_infix_binary_to_single_expr, map,
    parse_binary_expression, parse_expr_literal, parse_literal, parse_number,
    parse_unary_expression, predicate, BinaryExpr, BinaryOperator, Expr, Identifier, LiteralExpr,
    Number, Parser, StringLiteral, UnaryExpr, UnaryOperator,
};

fn mock_number_literal_expr(num: i32) -> Expr {
    Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(num)))
}

#[test]
fn test_map() {
    fn mapper(src: &str) -> usize {
        src.len()
    }

    let src = "130 150";
    let combined_parser = map(parse_number(), mapper);

    let result = combined_parser.parse(src).unwrap();

    assert_eq!((" 150", 3), result);
}

#[test]
fn test_any_char_and_pred() {
    let src = "1234 foo";
    let four_digit_num = predicate(parse_number(), |num| num.len() == 4);

    let result = four_digit_num.parse(src);

    assert_eq!(Ok((" foo", "1234")), result);

    assert!(four_digit_num.parse("123 foo").is_err());
}

#[test]
fn test_at_least_one_whitespace() {
    let src = "   foo = bar";
    let parser = at_least_one_whitespace();

    let result = parser.parse(src).unwrap();

    assert_eq!(("foo = bar", ()), result);

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
            (Expr::Binary(BinaryExpr {
                lhs: Box::new(Expr::Binary(BinaryExpr {
                    lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                        Number::Integer32(3)
                    ))),
                    rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                        Number::Integer32(100)
                    ))),
                    op: BinaryOperator::Mul
                })),
                rhs: Box::new(Expr::Binary(BinaryExpr {
                    lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                        Number::Integer32(4)
                    ))),
                    rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                        Number::Integer32(2)
                    ))),
                    op: BinaryOperator::Mul
                })),
                op: BinaryOperator::Plus
            }))
        ),
        result
    );
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

#[test]
fn test_parse_expr_literal() {
    let parser = parse_expr_literal();

    assert_eq!(
        parser.parse("foo").unwrap(),
        (
            "",
            Expr::Literal(LiteralExpr::Identifier(Identifier(String::from("foo"))))
        )
    );
    assert_eq!(
        parser.parse("123").unwrap(),
        (
            "",
            Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(123)))
        )
    );
}

#[test]
fn test_parse_unary_expr() {
    let parser = parse_unary_expression;

    assert_eq!(
        parser.parse("-3"),
        Ok((
            "",
            Expr::Unary(UnaryExpr {
                op: UnaryOperator::Minus,
                expr: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                    Number::Integer32(3)
                ))),
            })
        ))
    );

    assert_eq!(
        parser.parse("!foo"),
        Ok((
            "",
            Expr::Unary(UnaryExpr {
                op: UnaryOperator::Bang,
                expr: Box::new(Expr::Literal(LiteralExpr::Identifier(Identifier(
                    String::from("foo")
                )))),
            })
        ))
    );
}
