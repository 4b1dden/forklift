use std::collections::HashMap;

use crate::{
    grammar::{Declaration, Program},
    interpreter::{InterpreterResult, FL_T},
};

use super::eval_declaration;

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

    pub fn interpret_program(&mut self) {
        for declaration in self.source.iter() {
            let _ = eval_declaration(declaration, &mut self.global_env);
        }
    }
}
