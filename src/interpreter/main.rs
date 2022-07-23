use std::borrow::BorrowMut;
use std::cell::{RefCell, Ref};
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

use crate::parser::{Expr, Identifier};
use crate::{
    grammar::{Declaration, Program},
    interpreter::{FL_T_Callable, InterpreterResult, FL_T},
};

use super::{Callable_Native, FL_T_Callable_Body};

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

    fn ancestor(
        env: Rc<RefCell<Environment>>,
        depth: usize,
    ) -> InterpreterResult<Rc<RefCell<Environment>>> {
        let mut curr_env = env.clone();
        let mut i = 0;

        while i < depth {
            curr_env = curr_env
                .clone()
                .borrow()
                .enclosing
                .as_ref()
                .ok_or(String::from("max ancestor depth exceeded"))?
                .clone();
            i = i + 1;
        }

        Ok(curr_env.clone())
    }

    pub fn get_at(
        env: Rc<RefCell<Environment>>,
        key: &str,
        depth: usize,
    ) -> InterpreterResult<Rc<FL_T>> {
        println!("get:  {:#} depth: {:#}", key, depth);
        let maybe_relevant_ancestor = Environment::ancestor(env.clone(), depth);

        match maybe_relevant_ancestor {
            Ok(relevant_ancestor) => {
                println!("relevant ancestor: {:#?}", relevant_ancestor.clone());
                if let Some(val) = relevant_ancestor.clone().borrow().get(key.to_string()) {
                    Ok(val)
                } else {
                    Err(format!("{:#?} was not found", key))
                }
            }
            Err(e) => {
                println!("not found! {:#?}", &env);

                Err(String::from("foooBAr!"))
            }
        }
    }

    pub fn put_at(
        env: Rc<RefCell<Environment>>,
        key: String,
        val: Rc<FL_T>,
        depth: usize,
    ) -> InterpreterResult<Rc<FL_T>> {
        let maybe_relevant_ancestor = Environment::ancestor(env.clone(), depth);

        match maybe_relevant_ancestor {
            Ok(relevant_ancestor) => {
                RefCell::borrow_mut(&relevant_ancestor).put(key, val.clone());

                Ok(val)
            }
            Err(e) => {
                println!("not found put {:#?}", &env);

                Err(String::from("foo bar baz !"))
            }
        }
    }

    /// TODO: should this ever fail? maybe when we have consts
    pub fn put(&mut self, key: String, val: Rc<FL_T>) -> InterpreterResult<Rc<FL_T>> {
        self.bindings.insert(key, val.clone());

        Ok(val)
    }
}

#[derive(Debug, Clone)]
pub struct Interpreter<W: Write> {
    pub source: Program,
    pub global_env: Rc<RefCell<Environment>>,
    pub locals: HashMap<Expr, usize>, // depth
    pub environment: Rc<RefCell<Environment>>,
    pub writer: Rc<RefCell<W>>,
}

impl<W: Write> Interpreter<W> {
    pub fn new(source: Program, writer: W) -> Self {
        let globals = Rc::new(RefCell::new(Environment::new(None)));
        let mut me = Self {
            source,
            global_env: globals.clone(),
            locals: HashMap::new(),
            writer: Rc::new(RefCell::new(writer)),
            environment: globals
        };

        me.load_defaults();

        me
    }

    pub fn resolve(&mut self, expr: Expr, depth: usize) {
        self.locals.insert(expr, depth);
        // println!(
        //     "inserted into interpreter locals, locals: {:#?}",
        //     &self.locals
        // );
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

    pub fn inject_into_global_env(
        &self,
        key: String,
        val: Rc<FL_T>,
    ) -> InterpreterResult<Rc<FL_T>> {
        RefCell::borrow_mut(&self.global_env).put(key, val)
    }

    pub fn interpret_program(&mut self) -> InterpreterResult<()> {
        for declaration in self.source.iter() {
            self.eval_declaration(declaration, self.global_env.clone())?;
        }

        Ok(())
    }
}
