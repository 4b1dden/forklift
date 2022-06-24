use crate::grammar::{Declaration, Statement};
use crate::interpreter::{evaluate_expr, FL_T};

// TODO: change, this is for early dev purposes only
pub type InterpreterResult<T> = Result<T, String>;

pub fn eval_declaration(decl: Declaration) -> InterpreterResult<FL_T> {
    match decl {
        Declaration::Let(_) => Err(String::from("not supported yet lol sry its todo")),
        Declaration::Statement(statement) => match statement {
            Statement::Expr(expr) => Ok(evaluate_expr(expr)),
            _ => Err(String::from("todo sry")),
        },
    }
}
