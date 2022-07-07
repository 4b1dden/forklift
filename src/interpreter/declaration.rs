use std::borrow::BorrowMut;
use std::cell::{RefCell, RefMut};
use std::rc::Rc;

use crate::grammar::{Declaration, Statement};
use crate::interpreter::{
    evaluate_block, evaluate_expr, evaluate_fn_def, evaluate_if_block, evaluate_print_statement,
    evaluate_return_statement, evaluate_while_statement, Environment, FL_T,
};
use crate::parser::{LetBinding, Reassignment};

use super::desugar_for_loop_to_while_block;

// TODO: change, this is for early dev purposes only
pub type InterpreterResult<T> = Result<T, String>;

pub fn eval_declaration(
    decl: &Declaration,
    env: Rc<RefCell<Environment>>,
) -> InterpreterResult<FL_T> {
    match decl {
        Declaration::Let(let_binding) => evaluate_let_binding(let_binding, env),
        Declaration::Reassignment(reassignment) => evaluate_reassignment(reassignment, env),
        Declaration::Statement(statement) => evaluate_statement(statement, env),
    }
}

pub fn evaluate_statement(
    statement: &Statement,
    env: Rc<RefCell<Environment>>,
) -> InterpreterResult<FL_T> {
    match statement {
        Statement::Expr(expr) => evaluate_expr(expr, env.clone()),
        Statement::Print(expr_to_print) => evaluate_print_statement(expr_to_print, env.clone()),
        Statement::Block(declarations) => evaluate_block(declarations, env.clone()),
        Statement::If(if_block) => evaluate_if_block(if_block, env.clone()),
        Statement::WhileLoop(while_loop) => evaluate_while_statement(while_loop, env.clone()),
        Statement::ForLoop(for_loop) => {
            let desugared_block = desugar_for_loop_to_while_block(for_loop)?;
            evaluate_statement(&desugared_block, env)
        }
        Statement::FnDef(fn_def) => evaluate_fn_def(fn_def, env.clone()),
        Statement::Return(maybe_ret_expr) => {
            evaluate_return_statement(maybe_ret_expr.as_ref(), env.clone())
        }
    }
}

pub fn evaluate_let_binding(
    binding: &LetBinding,
    env: Rc<RefCell<Environment>>,
) -> InterpreterResult<FL_T> {
    let rhs_evaluated = evaluate_expr(&binding.rhs, env.clone())?;

    RefCell::borrow_mut(&env).put(binding.identifier.0.clone(), Rc::new(rhs_evaluated.clone()));

    Ok(rhs_evaluated)
}

/// TODO: We need to either get a mutable reference to the enclosing environment
/// or "capture" enclosed variables inside of the local environment, then mutate them locally and
/// also check the while loop condition locally
/// I am 95% sure this implementation will bite me in the ass in the future
pub fn evaluate_reassignment(
    reassignment: &Reassignment,
    env: Rc<RefCell<Environment>>,
) -> InterpreterResult<FL_T> {
    let new_val = evaluate_expr(&reassignment.rhs, env.clone())?;

    RefCell::borrow_mut(&env).put(reassignment.identifier.0.clone(), Rc::new(new_val.clone()))?;

    Ok(new_val)
}
