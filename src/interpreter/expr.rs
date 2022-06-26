use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Sub};

use crate::grammar::{Declaration, Statement};
use crate::interpreter::{eval_declaration, Environment};
use crate::parser::{
    BinaryExpr, BinaryOperator, Expr, IfBlock, LetBinding, LiteralExpr, UnaryExpr, UnaryOperator,
};

use super::{evaluate_statement, InterpreterResult};

pub fn evaluate_expr(expr: &Expr, env: &Environment) -> InterpreterResult<FL_T> {
    match expr {
        Expr::Literal(literal_expr) => evaluate_literal_expr(literal_expr, env),
        Expr::Unary(unary_expr) => evaluate_unary_expr(unary_expr, env),
        Expr::Binary(binary_expr) => evaluate_binary_expr(binary_expr, env),
        Expr::Grouping(grouping_body) => evaluate_grouping_expr(grouping_body, env),
    }
}

pub fn evaluate_print_statement(expr: &Expr, env: &Environment) -> InterpreterResult<FL_T> {
    let evaluated = evaluate_expr(expr, env)?;

    println!("{:?}", evaluated);

    Ok(evaluated)
}

pub fn evaluate_block(declarations: &[Declaration], env: &Environment) -> InterpreterResult<FL_T> {
    let mut local_env = Environment::new(Some(&env));

    let mut last_result = FL_T::Unit;
    for declaration in declarations.iter() {
        last_result = eval_declaration(declaration, &mut local_env)?;
    }

    return Ok(last_result);
}

pub fn evaluate_if_block(if_block: &IfBlock, env: &Environment) -> InterpreterResult<FL_T> {
    let determinant = evaluate_expr(&if_block.cond, env)?;
    let if_clause_condition = cast_to_bool(&determinant);
    let mut local_env = Environment::new(Some(&env));

    let mut last_result = FL_T::Unit;
    if if_clause_condition {
        last_result = evaluate_statement(&if_block.truthy_statement, &mut local_env)?;
    } else if if_block.else_statement.is_some() {
        last_result =
            evaluate_statement(if_block.else_statement.as_ref().unwrap(), &mut local_env)?;
    }

    Ok(last_result)
}

#[derive(Debug, Clone, PartialEq)]
pub enum FL_T {
    Primitive(FL_T_Primitive),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FL_T_Primitive {
    Str(String),
    Integer32(i32),
}

pub fn evaluate_literal_expr(expr: &LiteralExpr, env: &Environment) -> InterpreterResult<FL_T> {
    match expr {
        LiteralExpr::Identifier(ident) => env
            .get(ident.0.clone())
            .ok_or(format!("{} not found", ident.0))
            .cloned(),
        LiteralExpr::Keyword(keyword) => todo!(),
        LiteralExpr::NumberLiteral(num) => Ok(FL_T::Primitive(FL_T_Primitive::Integer32(num.0))),
        LiteralExpr::StringLiteral(s) => Ok(FL_T::Primitive(FL_T_Primitive::Str(s.0.clone()))),
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

pub fn evaluate_unary_expr(unary_expr: &UnaryExpr, env: &Environment) -> InterpreterResult<FL_T> {
    let evaluated_inside = evaluate_expr(&unary_expr.expr, env)?;

    match unary_expr.op {
        UnaryOperator::Minus => Ok(negate_fl_t(evaluated_inside)),
        UnaryOperator::Bang => todo!(),
    }
}

fn apply_bin_op_to_bin_expr(left: FL_T, op: &BinaryOperator, right: FL_T) -> FL_T {
    match (left, right) {
        (
            FL_T::Primitive(FL_T_Primitive::Integer32(l_num)),
            FL_T::Primitive(FL_T_Primitive::Integer32(r_num)),
        ) => FL_T::Primitive(FL_T_Primitive::Integer32(compute_bin_op(l_num, op, r_num))),
        _ => todo!(),
    }
}

fn compute_bin_op<T>(l: T, op: &BinaryOperator, r: T) -> T
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

pub fn evaluate_binary_expr(expr: &BinaryExpr, env: &Environment) -> InterpreterResult<FL_T> {
    let left = evaluate_expr(&expr.lhs, env)?;
    let right = evaluate_expr(&expr.rhs, env)?;
    let op = &expr.op;

    Ok(apply_bin_op_to_bin_expr(left, op, right))
}

pub fn evaluate_grouping_expr(expr: &Box<Expr>, env: &Environment) -> InterpreterResult<FL_T> {
    evaluate_expr(&expr, env)
}

// TODO: Remove this once we have type checks? I'd want my language to be strict and now allow
// stuff like if ("foo") like wtf is a "truthy" value they have played us for absolute fools
pub fn cast_to_bool(x: &FL_T) -> bool {
    match x {
        FL_T::Primitive(FL_T_Primitive::Integer32(int32)) => *int32 > 0,
        FL_T::Primitive(FL_T_Primitive::Str(string)) => string != "",
        FL_T::Unit => false,
    }
}

#[path = "expr.test.rs"]
mod tests;
