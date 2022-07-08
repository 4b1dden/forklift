use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::{Expr, Identifier};
use crate::{
    grammar::{Declaration, Program},
    interpreter::{FL_T_Callable, InterpreterResult, FL_T},
};

use super::{eval_declaration, Callable_Native, FL_T_Callable_Body};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub bindings: HashMap<String, Rc<FL_T>>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            bindings: HashMap::new(),
            enclosing,
        }
    }

    pub fn get(&self, key: String) -> Option<Rc<FL_T>> {
        match self.bindings.get(&key) {
            local_val @ Some(_) => local_val.cloned(),
            None => self
                .enclosing
                .as_ref()
                .and_then(|enclosed_env| enclosed_env.borrow().get(key)),
        }
    }

    /// TODO: should this ever fail? maybe when we have consts
    pub fn put(&mut self, key: String, val: Rc<FL_T>) -> InterpreterResult<()> {
        self.bindings.insert(key, val);

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub source: Program,
    pub global_env: Rc<RefCell<Environment>>,
    pub locals: HashMap<Expr, usize>, // depth
}

impl Interpreter {
    pub fn new(source: Program) -> Self {
        let mut me = Self {
            source,
            global_env: Rc::new(RefCell::new(Environment::new(None))),
            locals: HashMap::new(),
        };

        me.load_defaults();

        me
    }

    pub fn resolve(&mut self, expr: Expr, depth: usize) {
        self.locals.insert(expr, depth);
        println!(
            "inserted into interpreter locals, locals: {:#?}",
            &self.locals
        );
    }

    pub fn load_defaults(&mut self) -> InterpreterResult<()> {
        /*
        let mut defaults = HashMap::new();

        defaults.insert(
            String::from("clock"),
            Rc::new(FL_T::Callable(FL_T_Callable {
                identifier: Identifier(String::from("clock")),
                arguments: None,
                body: FL_T_Callable_Body::Native(Callable_Native::Clock),
                def_env: self.global_env.clone(),
            })),
        );

        self.preload_with(defaults)
        */

        Ok(())
    }

    pub fn preload_with(&mut self, m: HashMap<String, Rc<FL_T>>) -> InterpreterResult<()> {
        for (key, val) in m {
            self.inject_into_global_env(key, val)?;
        }

        Ok(())
    }

    pub fn inject_into_global_env(&mut self, key: String, val: Rc<FL_T>) -> InterpreterResult<()> {
        RefCell::borrow_mut(&self.global_env).put(key, val)
    }

    pub fn interpret_program(&mut self) -> InterpreterResult<()> {
        for declaration in self.source.iter() {
            eval_declaration(declaration, self.global_env.clone())?;
        }

        Ok(())
    }
}
