use std::collections::HashMap;

use crate::grammar::{Declaration, Statement};
use crate::interpreter::{evaluate_expr, FL_T};
use crate::parser::LetBinding;

// TODO: change, this is for early dev purposes only
pub type InterpreterResult<T> = Result<T, String>;

pub fn eval_declaration(decl: Declaration, env: &mut Environment) -> InterpreterResult<FL_T> {
    match decl {
        Declaration::Let(let_binding) => evaluate_let_binding(let_binding, env),
        Declaration::Statement(statement) => match statement {
            Statement::Expr(expr) => Ok(evaluate_expr(expr, &env)),
            _ => Err(String::from("todo sry")),
        },
    }
}

#[derive(Debug, Clone)]
pub struct Environment<'a> {
    pub bindings: HashMap<String, FL_T>,
    pub enclosing: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new(enclosing: Option<&'a Environment<'a>>) -> Self {
        Self {
            bindings: HashMap::new(),
            enclosing,
        }
    }

    pub fn get(&self, key: String) -> Option<&FL_T> {
        match self.bindings.get(&key) {
            local_val @ Some(_) => local_val,
            None => self
                .enclosing
                .and_then(|enclosed_env| enclosed_env.get(key)),
        }
    }

    pub fn put(&mut self, key: String, val: FL_T) -> InterpreterResult<FL_T> {
        self.bindings
            .insert(key, val)
            .ok_or(String::from("Could not insert into local bindings"))
    }
}

pub fn evaluate_let_binding(binding: LetBinding, env: &mut Environment) -> InterpreterResult<FL_T> {
    let rhs_evaluated = evaluate_expr(binding.rhs, env);

    let _ = env.put(binding.identifier.0, rhs_evaluated.clone());

    Ok(rhs_evaluated)
}
