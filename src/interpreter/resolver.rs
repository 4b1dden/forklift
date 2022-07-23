use std::{collections::HashMap, io::Write};

use crate::{
    grammar::{Declaration, Program, Statement},
    interpreter::{Interpreter, InterpreterResult},
    parser::{Expr, FnCall, FnDef, Identifier, LetBinding, LiteralExpr},
};

#[derive(Debug)]
pub struct Resolver<W: Write> {
    pub scopes: Vec<HashMap<String, bool>>,
    // this is not good, intepreter should be ideally handled differently
    pub interpreter: Interpreter<W>,
}

impl<W: Write> Resolver<W> {
    pub fn new(interpreter: Interpreter<W>) -> Self {
        Self {
            scopes: vec![],
            interpreter,
        }
    }

    pub fn resolve_block_statement(
        &mut self,
        declarations: Vec<Declaration>,
    ) -> InterpreterResult<()> {
        self.begin_scope();

        for declaration in declarations.iter() {
            self.resolve_declaration(declaration)?;
        }

        self.end_scope();

        Ok(())
    }

    fn begin_scope(&mut self) {
        //println!("pushing new scope");
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) -> Option<HashMap<String, bool>> {
        self.scopes.pop()
    }

    pub fn resolve_statement(&mut self, statement: Statement) -> InterpreterResult<()> {
        match statement {
            Statement::Block(declarations) => self.resolve_block_statement(declarations),
            Statement::FnDef(fn_def) => {
                self.declare(fn_def.identifier.clone())?;
                self.define(fn_def.identifier.clone())?;

                self.resolve_fn_def(&fn_def)
            }
            Statement::Expr(expr) => self.resolve_expr(&expr),
            Statement::If(if_clause) => {
                self.resolve_expr(&if_clause.cond)
                    .and_then(|_| self.resolve_statement(if_clause.truthy_statement.clone()))?;

                if let Some(else_statement) = if_clause.else_statement {
                    self.resolve_statement(else_statement)?;
                }

                Ok(())
            }
            Statement::Print(expr) => self.resolve_expr(&expr),
            Statement::Return(maybe_return_expr) => {
                if let Some(return_expr) = maybe_return_expr {
                    self.resolve_expr(&return_expr)
                } else {
                    Ok(())
                }
            }
            Statement::WhileLoop(while_loop) => {
                self.resolve_expr(&while_loop.condition)?;

                self.resolve_statement(while_loop.body)
            }
            _ => todo!(),
        }
    }

    pub fn resolve_declaration(&mut self, declaration: &Declaration) -> InterpreterResult<()> {
        match declaration {
            Declaration::Let(let_binding) => self.resolve_let_binding(let_binding),
            Declaration::Reassignment(reassignment) => {
                self.resolve_expr(&reassignment.rhs)?;

                self.resolve_local(&reassignment.rhs, &reassignment.identifier)
            }
            Declaration::Statement(statement) => self.resolve_statement(statement.clone()),
        }
    }

    pub fn resolve_expr(&mut self, expr: &Expr) -> InterpreterResult<()> {
        // println!("resolving {:#?}, self.scopes: {:#?}", expr, &self.scopes);
        match expr {
            Expr::Literal(literal_expr) => self.resolve_literal_expr(literal_expr),
            Expr::Unary(unary_expr) => self.resolve_expr(&*unary_expr.expr),
            Expr::Binary(binary_expr) => self
                .resolve_expr(&*binary_expr.lhs)
                .and_then(|_| self.resolve_expr(&*binary_expr.rhs)),
            Expr::Grouping(expr) => self.resolve_expr(&*expr),
            Expr::FnCall(fn_call) => self.resolve_fn_call(&fn_call),
        }
    }

    fn resolve_literal_expr(&mut self, literal_expr: &LiteralExpr) -> InterpreterResult<()> {
        match literal_expr {
            LiteralExpr::Identifier(identifier) => self.resolve_identifier(identifier),
            _ => Ok(()),
        }
    }

    fn resolve_fn_call(&mut self, fn_call: &FnCall) -> InterpreterResult<()> {
        self.resolve_expr(&fn_call.callee)?;

        if let Some(args) = &fn_call.arguments {
            for arg in args.iter() {
                self.resolve_expr(arg)?;
            }
        }

        Ok(())
    }

    fn resolve_identifier(&mut self, identifier: &Identifier) -> InterpreterResult<()> {
        // does this even work?
        let maybe_last = self.scopes.last();
        if let Some(last) = maybe_last {
            if let Some(decl) = last.get(&identifier.0) {
                if *decl == false && !self.scopes.is_empty() {
                    return Err(format!(
                        "Can not reference variable {:#} in it's own declaration",
                        identifier.0.clone()
                    ));
                }
            }
        }

        self.resolve_local(
            &Expr::Literal(LiteralExpr::Identifier(identifier.clone())),
            identifier,
        )?;

        Ok(())
    }

    fn resolve_let_binding(&mut self, let_binding: &LetBinding) -> InterpreterResult<()> {
        self.declare(let_binding.identifier.clone())?;

        self.resolve_expr(&let_binding.rhs)?;

        self.resolve_local(&let_binding.rhs, &let_binding.identifier)?;

        self.define(let_binding.identifier.clone())
    }

    fn resolve_fn_def(&mut self, fn_def: &FnDef) -> InterpreterResult<()> {
        self.begin_scope();

        if let Some(args) = &fn_def.arguments {
            for arg in args.iter() {
                self.declare(arg.clone())?;
                self.define(arg.clone())?;
            }
        }

        self.resolve_statement(fn_def.body.clone())?;

        self.end_scope();

        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expr, identifier: &Identifier) -> InterpreterResult<()> {
        let scope_size = self.scopes.len();
        if scope_size == 0 {
            // is a global var def
            //println!("{:#?}", &self.scopes);
            //println!(
            //    "scope size 0 when resolving local {:#?} {:#?}",
            //    expr, identifier
            //);
            return Ok(());
        }
        let mut i = scope_size - 1;

        while i >= 0 {
            // do
            if self.scopes.get(i).unwrap().get(&identifier.0).is_some() {
                /*
                println!(
                    "Found {:?} in {:?} scope, scope_size = {:?}",
                    &identifier.0, i, scope_size
                );
                */
                self.resolve_for_interpreter(expr, scope_size - 1 - i);

                return Ok(());
            }
            // println!("i = {:#}", i);

            if i != 0 {
                i = i - 1; // prevent underflow
            } else {
                break;
            }
        }

        Ok(())
    }

    fn resolve_for_interpreter(&mut self, expr: &Expr, depth: usize) {
        // println!("depth: {:?}", depth);
        self.interpreter.resolve(expr.clone(), depth)
    }

    fn declare(&mut self, name: Identifier) -> InterpreterResult<()> {
        if self.scopes.is_empty() {
            return Ok(());
            // return Err(String::from("Scopes empty"));
        }

        let target_scope = self.scopes.last_mut().unwrap();
        if target_scope.contains_key(&name.0) {
            return Err(format!("Local binding with the given identifier {:#?} already exists. Use a reassignment (e.g. `foo = 1` instead of `let foo = 1`", name.0.clone()));
        }

        self.scopes.last_mut().unwrap().insert(name.0, false);

        Ok(())
    }

    fn define(&mut self, name: Identifier) -> InterpreterResult<()> {
        if self.scopes.is_empty() {
            return Ok(()); // not sure about this one
        }

        self.scopes.last_mut().unwrap().insert(name.0, true);

        Ok(())
    }

    pub fn resolve_program(&mut self, program: Program) -> InterpreterResult<()> {
        for declaration in program.iter() {
            self.resolve_declaration(declaration)?;
        }

        Ok(())
    }
}
