use crate::grammar::{Declaration, Statement};
use crate::interpreter::{
    evaluate_block, evaluate_expr, evaluate_if_block, evaluate_print_statement, Environment, FL_T,
};
use crate::parser::LetBinding;

// TODO: change, this is for early dev purposes only
pub type InterpreterResult<T> = Result<T, String>;

pub fn eval_declaration(decl: &Declaration, env: &mut Environment) -> InterpreterResult<FL_T> {
    match decl {
        Declaration::Let(let_binding) => evaluate_let_binding(let_binding, env),
        Declaration::Statement(statement) => evaluate_statement(statement, env),
    }
}

pub fn evaluate_statement(statement: &Statement, env: &mut Environment) -> InterpreterResult<FL_T> {
    match statement {
        Statement::Expr(expr) => evaluate_expr(expr, &env),
        Statement::Print(expr_to_print) => evaluate_print_statement(expr_to_print, &env),
        Statement::Block(declarations) => evaluate_block(declarations, &env),
        Statement::If(if_block) => evaluate_if_block(if_block, &env),
    }
}

pub fn evaluate_let_binding(
    binding: &LetBinding,
    env: &mut Environment,
) -> InterpreterResult<FL_T> {
    let rhs_evaluated = evaluate_expr(&binding.rhs, env)?;

    let _ = env.put(binding.identifier.0.clone(), rhs_evaluated.clone());

    Ok(rhs_evaluated)
}
