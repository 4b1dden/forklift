use std::ops::{Add, Div, Mul, Sub};

use crate::parser::{BinaryExpr, BinaryOperator, Expr, LiteralExpr, UnaryExpr, UnaryOperator};

pub fn evaluate_expr(expr: Expr) -> FL_T {
    match expr {
        Expr::Literal(literal_expr) => evaluate_literal_expr(literal_expr),
        Expr::Unary(unary_expr) => evaluate_unary_expr(unary_expr),
        Expr::Binary(binary_expr) => evaluate_binary_expr(binary_expr),
        Expr::Grouping(grouping_body) => evaluate_grouping_expr(grouping_body),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FL_T {
    Primitive(FL_T_Primitive),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FL_T_Primitive {
    Str(String),
    Integer32(i32),
}

pub fn evaluate_literal_expr(expr: LiteralExpr) -> FL_T {
    match expr {
        LiteralExpr::Identifier(ident) => todo!(),
        LiteralExpr::Keyword(keyword) => todo!(),
        LiteralExpr::NumberLiteral(num) => FL_T::Primitive(FL_T_Primitive::Integer32(num.0)),
        LiteralExpr::StringLiteral(s) => FL_T::Primitive(FL_T_Primitive::Str(s.0)),
        LiteralExpr::Empty => todo!(),
    }
}

fn negate_fl_t(t: FL_T) -> FL_T {
    match t {
        // it'd be nice to .map here
        FL_T::Primitive(FL_T_Primitive::Integer32(num)) => {
            FL_T::Primitive(FL_T_Primitive::Integer32(-num))
        }
        _ => todo!(),
    }
}

pub fn evaluate_unary_expr(unary_expr: UnaryExpr) -> FL_T {
    let evaluated_inside = evaluate_expr(*unary_expr.expr);

    match unary_expr.op {
        UnaryOperator::Minus => negate_fl_t(evaluated_inside),
        UnaryOperator::Bang => todo!(),
    }
}

fn apply_bin_op_to_bin_expr(left: FL_T, op: BinaryOperator, right: FL_T) -> FL_T {
    match (left, right) {
        (
            FL_T::Primitive(FL_T_Primitive::Integer32(l_num)),
            FL_T::Primitive(FL_T_Primitive::Integer32(r_num)),
        ) => FL_T::Primitive(FL_T_Primitive::Integer32(compute_bin_op(l_num, op, r_num))),
        _ => todo!(),
    }
}

fn compute_bin_op<T>(l: T, op: BinaryOperator, r: T) -> T
where
    T: Mul<Output = T>,
    T: Div<Output = T>,
    T: Add<Output = T>,
    T: Sub<Output = T>,
{
    match op {
        BinaryOperator::Minus => l - r,
        BinaryOperator::Plus => l + r,
        BinaryOperator::Mul => l * r,
        BinaryOperator::Div => l / r,
    }
}

pub fn evaluate_binary_expr(expr: BinaryExpr) -> FL_T {
    let left = evaluate_expr(*expr.lhs);
    let right = evaluate_expr(*expr.rhs);
    let op = expr.op;

    apply_bin_op_to_bin_expr(left, op, right)
}

pub fn evaluate_grouping_expr(expr: Box<Expr>) -> FL_T {
    evaluate_expr(*expr)
}

#[path = "expr.test.rs"]
mod tests;
