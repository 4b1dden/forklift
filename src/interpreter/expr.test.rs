use super::{evaluate_expr, FL_T_Primitive, FL_T};
use crate::interpreter::Environment;
use crate::parser::{
    BinaryExpr, BinaryOperator, Expr, LiteralExpr, Number, StringLiteral, UnaryExpr, UnaryOperator,
};

fn empty_env() -> Environment<'static> {
    let ENV = Environment::new(None);

    ENV
}

#[test]
fn test_literal_expr() {
    let to_intepret = Expr::Literal(LiteralExpr::NumberLiteral(Number(3)));

    assert_eq!(
        evaluate_expr(&to_intepret, &empty_env()),
        Ok(FL_T::Primitive(FL_T_Primitive::Integer32(3)))
    );
}

#[test]
fn test_unary_expr() {
    let inner = Expr::Unary(UnaryExpr {
        op: UnaryOperator::Minus,
        expr: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(5)))),
    });

    let to_intepret = inner.clone();
    let nested = Expr::Unary(UnaryExpr {
        op: UnaryOperator::Minus,
        expr: Box::new(inner.clone()),
    });

    assert_eq!(
        evaluate_expr(&to_intepret, &empty_env()),
        Ok(FL_T::Primitive(FL_T_Primitive::Integer32(-5)))
    );

    assert_eq!(
        evaluate_expr(&nested, &empty_env()),
        Ok(FL_T::Primitive(FL_T_Primitive::Integer32(5)))
    );
}

#[test]
fn test_binary_expr() {
    // TODO: extend test cases
    let inner = Expr::Binary(BinaryExpr {
        lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(5)))),
        rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(10)))),
        op: BinaryOperator::Mul,
    });

    let nested = Expr::Binary(BinaryExpr {
        lhs: Box::new(inner.clone()),
        rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(Number(3)))),
        op: BinaryOperator::Plus,
    });

    assert_eq!(
        evaluate_expr(&inner, &empty_env()),
        Ok(FL_T::Primitive(FL_T_Primitive::Integer32(50)))
    );

    assert_eq!(
        evaluate_expr(&nested, &empty_env()),
        Ok(FL_T::Primitive(FL_T_Primitive::Integer32(53)))
    );
}