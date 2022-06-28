use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::{Add, Div, Mul, Sub};

use crate::grammar::{Declaration, Statement};
use crate::interpreter::{eval_declaration, Environment};
use crate::parser::{
    BinaryExpr, BinaryOperator, Expr, IfBlock, LetBinding, LiteralExpr, Number, UnaryExpr,
    UnaryOperator, WhileLoop,
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

    println!("{}", evaluated);

    Ok(evaluated)
}

pub fn evaluate_block(
    declarations: &[Declaration],
    env: &mut Environment,
) -> InterpreterResult<FL_T> {
    let mut local_env = Environment::new(Some(&env));
    local_env.bindings = env.bindings.clone();

    let mut last_result = FL_T::Unit;
    for declaration in declarations.iter() {
        last_result = eval_declaration(declaration, env)?;
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

pub fn evaluate_while_statement(
    while_loop: &WhileLoop,
    env: &mut Environment,
) -> InterpreterResult<FL_T> {
    let mut determinant;
    let mut determinant_as_bool;
    let mut last_result: InterpreterResult<FL_T> = Ok(FL_T::Unit);
    let mut local_env = Environment::new(Some(&env));

    loop {
        determinant = evaluate_expr(&while_loop.condition, env)?;
        determinant_as_bool = cast_to_bool(&determinant);

        if determinant_as_bool {
            last_result = evaluate_statement(&while_loop.body, env);
        } else {
            break; // while loop cond is no longer true
        }
    }

    last_result
}

#[derive(Debug, Clone, PartialEq)]
pub enum FL_T {
    Primitive(FL_T_Primitive),
    Unit,
}

impl Display for FL_T {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FL_T::Primitive(FL_T_Primitive::Str(string)) => {
                write!(f, "{}", string)
            }
            FL_T::Primitive(FL_T_Primitive::Integer32(int32)) => {
                write!(f, "{}", int32)
            }
            FL_T::Primitive(FL_T_Primitive::Bool(b)) => {
                write!(f, "{}", b)
            }
            FL_T::Primitive(FL_T_Primitive::Float64(float64)) => {
                write!(f, "{}", float64)
            }
            FL_T::Unit => {
                write!(f, "()")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FL_T_Primitive {
    Str(String),
    Bool(bool),
    Integer32(i32),
    Float64(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FL_T_Bool {
    True,
    False,
}

pub fn evaluate_literal_expr(expr: &LiteralExpr, env: &Environment) -> InterpreterResult<FL_T> {
    match expr {
        LiteralExpr::Identifier(ident) => env
            .get(ident.0.clone())
            .ok_or(format!("{} not found", ident.0))
            .cloned(),
        LiteralExpr::Keyword(keyword) => todo!(),
        LiteralExpr::NumberLiteral(Number::Integer32(num)) => {
            Ok(FL_T::Primitive(FL_T_Primitive::Integer32(*num)))
        }
        LiteralExpr::NumberLiteral(Number::Float64(float64)) => {
            Ok(FL_T::Primitive(FL_T_Primitive::Float64(*float64)))
        }
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

fn apply_bin_op_to_bin_expr(
    left: FL_T,
    op: &BinaryOperator,
    right: FL_T,
) -> InterpreterResult<FL_T> {
    match (left, right) {
        (
            FL_T::Primitive(FL_T_Primitive::Integer32(l_num)),
            FL_T::Primitive(FL_T_Primitive::Integer32(r_num)),
        ) => Ok(compute_bin_op(l_num, op, r_num)),
        (
            FL_T::Primitive(FL_T_Primitive::Str(l_str)),
            FL_T::Primitive(FL_T_Primitive::Str(r_str)),
        ) => apply_str_bin_op(l_str, op, r_str),
        (
            FL_T::Primitive(FL_T_Primitive::Float64(l_f64)),
            FL_T::Primitive(FL_T_Primitive::Float64(r_f64)),
        ) => Ok(compute_bin_op_f64(l_f64, op, r_f64)),
        _ => todo!("apply_bin_op_to_bin_expr"),
    }
}

// TODO: expand back to generics later probably
fn compute_bin_op(l: i32, op: &BinaryOperator, r: i32) -> FL_T {
    match op {
        BinaryOperator::Minus => FL_T::Primitive(FL_T_Primitive::Integer32(l - r)),
        BinaryOperator::Plus => FL_T::Primitive(FL_T_Primitive::Integer32(l + r)),
        BinaryOperator::Mul => FL_T::Primitive(FL_T_Primitive::Integer32(l * r)),
        BinaryOperator::Div => FL_T::Primitive(FL_T_Primitive::Integer32(l / r)),

        BinaryOperator::Greater => downcast_bin_comparison_to_fl_t_i32(|| l > r),
        BinaryOperator::GreaterEq => downcast_bin_comparison_to_fl_t_i32(|| l >= r),
        BinaryOperator::Less => downcast_bin_comparison_to_fl_t_i32(|| l < r),
        BinaryOperator::LessEq => downcast_bin_comparison_to_fl_t_i32(|| l <= r),
        BinaryOperator::BangEq => downcast_bin_comparison_to_fl_t_i32(|| l != r),
        BinaryOperator::EqEq => downcast_bin_comparison_to_fl_t_i32(|| l == r),
    }
}

fn compute_bin_op_f64(l: f64, op: &BinaryOperator, r: f64) -> FL_T {
    match op {
        BinaryOperator::Minus => FL_T::Primitive(FL_T_Primitive::Float64(l - r)),
        BinaryOperator::Plus => FL_T::Primitive(FL_T_Primitive::Float64(l + r)),
        BinaryOperator::Mul => FL_T::Primitive(FL_T_Primitive::Float64(l * r)),
        BinaryOperator::Div => FL_T::Primitive(FL_T_Primitive::Float64(l / r)),

        BinaryOperator::Greater => downcast_bin_comparison_to_fl_t_i32(|| l > r),
        BinaryOperator::GreaterEq => downcast_bin_comparison_to_fl_t_i32(|| l >= r),
        BinaryOperator::Less => downcast_bin_comparison_to_fl_t_i32(|| l < r),
        BinaryOperator::LessEq => downcast_bin_comparison_to_fl_t_i32(|| l <= r),
        BinaryOperator::BangEq => downcast_bin_comparison_to_fl_t_i32(|| l != r),
        BinaryOperator::EqEq => downcast_bin_comparison_to_fl_t_i32(|| l == r),
    }
}

fn downcast_bin_comparison_to_fl_t_i32<F>(f: F) -> FL_T
where
    F: Fn() -> bool,
{
    const TRUE_INT_VAL: i32 = 1;
    const FALSE_INT_VAL: i32 = 0;

    if f() {
        FL_T::Primitive(FL_T_Primitive::Integer32(TRUE_INT_VAL))
    } else {
        FL_T::Primitive(FL_T_Primitive::Integer32(FALSE_INT_VAL))
    }
}

pub fn apply_str_bin_op(
    l_str: String,
    op: &BinaryOperator,
    r_str: String,
) -> InterpreterResult<FL_T> {
    match op {
        BinaryOperator::Plus => Ok(FL_T::Primitive(FL_T_Primitive::Str(String::from(
            l_str + &r_str,
        )))),
        // probably not the most optimal way to concat strs because of double conversion but w/e
        _ => Err(format!("Can not apply {:#?} to String and String", op)),
    }
}

pub fn evaluate_binary_expr(expr: &BinaryExpr, env: &Environment) -> InterpreterResult<FL_T> {
    let left = evaluate_expr(&expr.lhs, env)?;
    let right = evaluate_expr(&expr.rhs, env)?;
    let op = &expr.op;

    apply_bin_op_to_bin_expr(left, op, right)
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
        FL_T::Primitive(FL_T_Primitive::Bool(flag)) => *flag,
        FL_T::Primitive(FL_T_Primitive::Float64(float64)) => *float64 > 0_f64,
        FL_T::Unit => false,
    }
}

#[path = "expr.test.rs"]
mod tests;
