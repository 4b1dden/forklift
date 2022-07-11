use std::borrow::BorrowMut;
use std::cell::{RefCell, RefMut};
use std::io::Write;
use std::rc::Rc;

use crate::grammar::{Declaration, Statement};
use crate::interpreter::{Environment, FL_T};
use crate::parser::{ensure_is_identifier, Expr, LetBinding, LiteralExpr, Reassignment};

use super::{desugar_for_loop_to_while_block, Interpreter};

// TODO: change, this is for early dev purposes only
pub type InterpreterResult<T> = Result<T, String>;

impl<W: Write> Interpreter<W> {
    pub fn eval_declaration(
        &self,
        decl: &Declaration,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<Rc<FL_T>> {
        match decl {
            Declaration::Let(let_binding) => self.evaluate_let_binding(let_binding, env),
            Declaration::Reassignment(reassignment) => {
                self.evaluate_reassignment(reassignment, env)
            }
            Declaration::Statement(statement) => {
                // TODO: unify so we dont need to wrap in Rc here
                self.evaluate_statement(statement, env).map(Rc::new)
            }
        }
    }

    pub fn evaluate_statement(
        &self,
        statement: &Statement,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        match statement {
            Statement::Expr(expr) => self.evaluate_expr(expr, env.clone()),
            Statement::Print(expr_to_print) => {
                self.evaluate_print_statement(expr_to_print, env.clone())
            }
            Statement::Block(declarations) => self.evaluate_block(declarations, env.clone()),
            Statement::If(if_block) => self.evaluate_if_block(if_block, env.clone()),
            Statement::WhileLoop(while_loop) => {
                self.evaluate_while_statement(while_loop, env.clone())
            }
            Statement::ForLoop(for_loop) => {
                let desugared_block = desugar_for_loop_to_while_block(for_loop)?;
                self.evaluate_statement(&desugared_block, env)
            }
            Statement::FnDef(fn_def) => self.evaluate_fn_def(fn_def, env.clone()),
            Statement::Return(maybe_ret_expr) => {
                self.evaluate_return_statement(maybe_ret_expr.as_ref(), env.clone())
            }
        }
    }

    pub fn evaluate_let_binding(
        &self,
        binding: &LetBinding,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<Rc<FL_T>> {
        let reconstructed = Expr::Literal(LiteralExpr::Identifier(binding.identifier.clone()));
        let maybe_depth = self.locals.get(&reconstructed);
        let rhs_evaluated = self.evaluate_expr(&binding.rhs, env.clone())?;

        if let Some(depth) = maybe_depth {
            // var is local
            Environment::put_at(
                env,
                binding.identifier.0.clone(),
                Rc::new(rhs_evaluated),
                *depth,
            )
        } else {
            // var is global
            RefCell::borrow_mut(&self.global_env)
                .put(binding.identifier.0.clone(), Rc::new(rhs_evaluated))
        }
    }

    /// TODO: We need to either get a mutable reference to the enclosing environment
    /// or "capture" enclosed variables inside of the local environment, then mutate them locally and
    /// also check the while loop condition locally
    /// I am 95% sure this implementation will bite me in the ass in the future
    pub fn evaluate_reassignment(
        &self,
        reassignment: &Reassignment,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<Rc<FL_T>> {
        let reconstructed = Expr::Literal(LiteralExpr::Identifier(reassignment.identifier.clone()));
        let maybe_depth = self.locals.get(&reconstructed);
        let rhs_evaluated = self.evaluate_expr(&reassignment.rhs, env.clone())?;
        let rhs_rc = Rc::new(rhs_evaluated);

        if let Some(depth) = maybe_depth {
            // reassignment of a local var
            Environment::put_at(env, reassignment.identifier.0.clone(), rhs_rc, *depth)
        } else {
            // reassignment of a global var
            RefCell::borrow_mut(&self.global_env).put(reassignment.identifier.0.clone(), rhs_rc)
        }
    }
}
