use std::collections::HashMap;

use crate::parser::Identifier;
use crate::{
    grammar::{Declaration, Program},
    interpreter::{FL_T_Callable, InterpreterResult, FL_T},
};

use super::{eval_declaration, Callable_Native, FL_T_Callable_Body};

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

    /// TODO: should this ever fail? maybe when we have consts
    pub fn put(&mut self, key: String, val: FL_T) -> InterpreterResult<()> {
        self.bindings.insert(key, val);

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Interpreter<'a> {
    pub source: Program,
    pub global_env: Environment<'a>,
}

impl<'a> Interpreter<'a> {
    pub fn new(source: Program) -> Self {
        Self {
            source,
            global_env: Environment::new(None),
        }
    }

    pub fn load_defaults(&mut self) -> InterpreterResult<()> {
        let mut defaults = HashMap::new();

        defaults.insert(
            String::from("clock"),
            FL_T::Callable(FL_T_Callable {
                identifier: Identifier(String::from("clock")),
                arguments: None,
                body: FL_T_Callable_Body::Native(Callable_Native::Clock),
            }),
        );

        self.preload_with(defaults)
    }

    pub fn preload_with(&mut self, m: HashMap<String, FL_T>) -> InterpreterResult<()> {
        for (key, val) in m {
            self.inject_into_global_env(key, val)?;
        }

        Ok(())
    }

    pub fn inject_into_global_env(&mut self, key: String, val: FL_T) -> InterpreterResult<()> {
        self.global_env.put(key, val)
    }

    pub fn interpret_program(&mut self) -> InterpreterResult<()> {
        for declaration in self.source.iter() {
            eval_declaration(declaration, &mut self.global_env)?;
        }

        Ok(())
    }
}
