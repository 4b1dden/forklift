use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::io::Write;
use std::ops::{Add, Deref, Div, Mul, Sub};
use std::rc::{Rc, Weak};
use std::time::{SystemTime, UNIX_EPOCH};

use ordered_float::OrderedFloat;

use crate::grammar::{Declaration, Statement};
use crate::interpreter::Environment;
use crate::parser::{
    BinaryExpr, BinaryOperator, Expr, FnCall, FnDef, ForLoop, Identifier, IfBlock, LetBinding,
    LiteralExpr, Number, UnaryExpr, UnaryOperator, WhileLoop,
};

use super::{Interpreter, InterpreterResult};

impl<W: Write> Interpreter<W> {
    pub fn evaluate_expr(
        &self,
        expr: &Expr,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        match expr {
            Expr::Literal(literal_expr) => self.evaluate_literal_expr(literal_expr, env),
            Expr::Unary(unary_expr) => self.evaluate_unary_expr(unary_expr, env),
            Expr::Binary(binary_expr) => self.evaluate_binary_expr(binary_expr, env),
            Expr::Grouping(grouping_body) => self.evaluate_grouping_expr(grouping_body, env),
            Expr::FnCall(fn_call) => self.evaluate_fn_call(fn_call, env),
        }
    }

    pub fn evaluate_print_statement(
        &self,
        expr: &Expr,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        let evaluated = self.evaluate_expr(expr, env)?;

        let to_write = evaluated.to_string() + "\n";

        RefCell::borrow_mut(&self.writer)
            .write(to_write.as_bytes())
            .map_err(|e| format!("Writer error: {:#?}", e))?;

        Ok(evaluated)
    }

    pub fn evaluate_block(
        &self,
        declarations: &[Declaration],
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        let local_env = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        RefCell::borrow_mut(&local_env).bindings = env.borrow().bindings.clone();

        let mut last_result = FL_T::Unit;
        for declaration in declarations.iter() {
            let rc_fl_t = self.eval_declaration(declaration, env.clone())?;

            // TODO: this deref is bad, i need to unify
            last_result = rc_fl_t.deref().clone();
        }

        return Ok(last_result);
    }

    pub fn evaluate_if_block(
        &self,
        if_block: &IfBlock,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        let determinant = self.evaluate_expr(&if_block.cond, env.clone())?;
        let if_clause_condition = cast_to_bool(&determinant);
        let local_env = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));

        let mut last_result = FL_T::Unit;
        if if_clause_condition {
            last_result = self.evaluate_statement(&if_block.truthy_statement, local_env)?;
        } else if if_block.else_statement.is_some() {
            last_result =
                self.evaluate_statement(if_block.else_statement.as_ref().unwrap(), local_env)?;
        }

        Ok(last_result)
    }

    pub fn evaluate_while_statement(
        &self,
        while_loop: &WhileLoop,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        let mut determinant;
        let mut determinant_as_bool;
        let mut last_result: InterpreterResult<FL_T> = Ok(FL_T::Unit);
        let mut local_env = Environment::new(Some(env.clone()));

        loop {
            determinant = self.evaluate_expr(&while_loop.condition, env.clone())?;
            determinant_as_bool = cast_to_bool(&determinant);

            if determinant_as_bool {
                last_result = self.evaluate_statement(&while_loop.body, env.clone());
            } else {
                break; // while loop cond is no longer true
            }
        }

        last_result
    }

    pub fn evaluate_fn_def(
        &self,
        fn_def: &FnDef,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        check_fn_def(fn_def)?;

        let fl_t_callable = FL_T_Callable::from_fn_def(fn_def, Rc::downgrade(&env));

        let res = RefCell::borrow_mut(&env).put(
            fn_def.identifier.0.clone(),
            Rc::new(FL_T::Callable(fl_t_callable)),
        );

        Ok(FL_T::Unit)
    }

    pub fn evaluate_return_statement(
        &self,
        maybe_return_expr: Option<&Expr>,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        match maybe_return_expr {
            None => Ok(FL_T::Primitive(FL_T_Primitive::Nil)),
            Some(return_expr) => self.evaluate_expr(return_expr, env),
        }
    }

    pub fn evaluate_unary_expr(
        &self,
        unary_expr: &UnaryExpr,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        let evaluated_inside = self.evaluate_expr(&unary_expr.expr, env)?;

        match unary_expr.op {
            UnaryOperator::Minus => Ok(negate_fl_t(evaluated_inside)),
            UnaryOperator::Bang => todo!(),
        }
    }

    pub fn evaluate_binary_expr(
        &self,
        expr: &BinaryExpr,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        let left = self.evaluate_expr(&expr.lhs, env.clone())?;
        let right = self.evaluate_expr(&expr.rhs, env.clone())?;
        let op = &expr.op;

        apply_bin_op_to_bin_expr(left, op, right)
    }

    pub fn evaluate_grouping_expr(
        &self,
        expr: &Box<Expr>,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        self.evaluate_expr(&expr, env)
    }

    pub fn evaluate_fn_call(
        &self,
        fn_call: &FnCall,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        let callee_eval = self.evaluate_expr(&fn_call.callee, env.clone())?;

        match callee_eval {
            FL_T::Callable(callable_def) => {
                // check if arg len match
                let callable_def_args = &callable_def.arguments;
                if fn_call.arity() != callable_def.arity() {
                    return Err(format!(
                        "Arguments count mismatch for {:?} call",
                        fn_call.callee.clone()
                    ));
                }

                // fill env for function local scope

                let local_fn_call_scope = callable_def.def_env.upgrade().unwrap_or_else(|| {
                    println!("[FL] Weak does not live anymore. Creating fallback env");
                    Rc::new(RefCell::new(Environment::new(Some(env.clone()))))
                });

                if let Some(fn_call_args) = &fn_call.arguments {
                    for (idx, fn_call_arg) in fn_call_args.iter().enumerate() {
                        let fn_def_corresponding_arg_name =
                            callable_def_args.as_ref().unwrap().get(idx).unwrap();

                        let evaluated_fn_call_arg = self.evaluate_expr(fn_call_arg, env.clone())?;

                        // fill new local scope with evaluated values
                        RefCell::borrow_mut(&local_fn_call_scope).put(
                            fn_def_corresponding_arg_name.0.clone(),
                            Rc::new(evaluated_fn_call_arg),
                        )?;
                    }
                }

                // evaluate function body with new function scope env
                match callable_def.body {
                    FL_T_Callable_Body::FL_T(body_statement) => {
                        self.evaluate_statement(&body_statement, local_fn_call_scope.clone())
                    }
                    FL_T_Callable_Body::Native(native_fn_variant) => {
                        evaluate_native_fn_variant(native_fn_variant, local_fn_call_scope.clone())
                    }
                }
            }
            _ => Err(format!("{:?} is not callable", callee_eval)),
        }
    }

    pub fn evaluate_literal_expr(
        &self,
        expr: &LiteralExpr,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<FL_T> {
        match expr {
            LiteralExpr::Identifier(ident) => {
                self.look_up_var(ident, env).map(|x| x.deref().clone()) // TODO: change
            }
            LiteralExpr::NumberLiteral(Number::Integer32(num)) => {
                Ok(FL_T::Primitive(FL_T_Primitive::Integer32(*num)))
            }
            LiteralExpr::NumberLiteral(Number::Float64(OrderedFloat(float64))) => {
                Ok(FL_T::Primitive(FL_T_Primitive::Float64(*float64)))
            }
            LiteralExpr::StringLiteral(s) => Ok(FL_T::Primitive(FL_T_Primitive::Str(s.0.clone()))),
            LiteralExpr::Empty => todo!(),
        }
    }

    pub fn look_up_var(
        &self,
        identifier: &Identifier,
        env: Rc<RefCell<Environment>>,
    ) -> InterpreterResult<Rc<FL_T>> {
        let reconstructed_var_expr = Expr::Literal(LiteralExpr::Identifier(identifier.clone()));

        let maybe_depth = self.locals.get(&reconstructed_var_expr);
        /*
        .ok_or(format!(
            "{:?} was not found in local resolver map",
            identifier.0.clone()
        ))?;
        */

        if let Some(depth) = maybe_depth {
            // var is local
            Environment::get_at(env, &identifier.0, *depth)
        } else {
            // var is global
            self.global_env
                .borrow()
                .get(identifier.0.clone())
                .ok_or(format!(
                    "{:#?} was thought to be a global but it is not there",
                    identifier.0.clone()
                ))
        }
    }
}

fn check_fn_def(fn_def: &FnDef) -> InterpreterResult<()> {
    const MAX_ARG_LEN: usize = 255;

    if let Some(arguments) = &fn_def.arguments {
        if arguments.len() > MAX_ARG_LEN {
            return Err(format!(
                "Maximum arg len exceeded. You have {}, max allowed is {}",
                arguments.len(),
                MAX_ARG_LEN
            ));
        }
    }

    Ok(())
}

pub fn desugar_for_loop_to_while_block(for_loop: &ForLoop) -> InterpreterResult<Statement> {
    let mut decls = Vec::<Declaration>::new();

    let while_loop_body = Statement::Block(vec![
        Declaration::Statement(for_loop.body.clone()),
        for_loop.post_iteration.clone(),
    ]);

    let inner_while = Statement::WhileLoop(Box::new(WhileLoop {
        condition: for_loop.condition.clone(),
        body: while_loop_body,
    }));

    decls.push(for_loop.init_declaration.clone());
    decls.push(Declaration::Statement(inner_while));

    Ok(Statement::Block(decls))
}

#[derive(Debug, Clone, PartialEq)]
pub enum FL_T {
    Primitive(FL_T_Primitive),
    Callable(FL_T_Callable),
    Unit,
}

impl Display for FL_T {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FL_T::Primitive(FL_T_Primitive::Str(string)) => {
                write!(f, "{}", string)
            }
            FL_T::Primitive(FL_T_Primitive::Integer32(int32)) => {
                write!(f, "{}", int32)
            }
            FL_T::Primitive(FL_T_Primitive::Bool(b)) => {
                write!(f, "{}", b)
            }
            FL_T::Primitive(FL_T_Primitive::Float64(float64)) => {
                write!(f, "{}", float64)
            }
            FL_T::Primitive(FL_T_Primitive::UInteger128(uint128)) => {
                write!(f, "{}", uint128)
            }
            FL_T::Primitive(FL_T_Primitive::Nil) => {
                write!(f, "nil")
            }
            FL_T::Callable(fl_t_callable) => {
                let ident = fl_t_callable.identifier.0.clone();
                write!(
                    f,
                    "<fn: {:?}({:?})>",
                    ident,
                    fl_t_callable
                        .arguments
                        .as_ref()
                        .map(|args| args.iter().fold(String::new(), |acc, curr| acc + &curr.0))
                )
            }
            FL_T::Unit => {
                write!(f, "()")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FL_T_Primitive {
    Str(String),
    Bool(bool),
    Integer32(i32),
    Float64(f64),
    UInteger128(u128),
    Nil,
}

#[derive(Debug, Clone)]
pub struct FL_T_Callable {
    pub identifier: Identifier,
    pub arguments: Option<Vec<Identifier>>, // ?
    pub body: FL_T_Callable_Body,
    pub def_env: Weak<RefCell<Environment>>,
}

impl PartialEq for FL_T_Callable {
    fn eq(&self, other: &Self) -> bool {
        self.identifier.eq(&other.identifier)
            && self.arguments.eq(&other.arguments)
            && self.body.eq(&other.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FL_T_Callable_Body {
    Native(Callable_Native), // Rust
    FL_T(Statement),         // FL_T
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callable_Native {
    Clock,
}

impl FL_T_Callable {
    fn from_fn_def(incoming: &FnDef, def_env: Weak<RefCell<Environment>>) -> Self {
        Self {
            identifier: incoming.identifier.clone(),
            arguments: incoming.arguments.clone(),
            body: FL_T_Callable_Body::FL_T(incoming.body.clone()),
            def_env,
        }
    }
}

pub trait Arity {
    fn arity(&self) -> usize;
}

impl Arity for &FnCall {
    fn arity(&self) -> usize {
        match &self.arguments {
            None => 0,
            Some(args) => args.len(),
        }
    }
}

impl Arity for FL_T_Callable {
    fn arity(&self) -> usize {
        match &self.arguments {
            None => 0,
            Some(args) => args.len(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FL_T_Bool {
    True,
    False,
}

fn negate_fl_t(t: FL_T) -> FL_T {
    match t {
        // it'd be nice to .map here
        FL_T::Primitive(FL_T_Primitive::Integer32(num)) => {
            FL_T::Primitive(FL_T_Primitive::Integer32(-num))
        }
        _ => todo!(),
    }
}

fn apply_bin_op_to_bin_expr(
    left: FL_T,
    op: &BinaryOperator,
    right: FL_T,
) -> InterpreterResult<FL_T> {
    match (left, right) {
        (
            FL_T::Primitive(FL_T_Primitive::Integer32(l_num)),
            FL_T::Primitive(FL_T_Primitive::Integer32(r_num)),
        ) => Ok(compute_bin_op(l_num, op, r_num)),
        (
            FL_T::Primitive(FL_T_Primitive::Str(l_str)),
            FL_T::Primitive(FL_T_Primitive::Str(r_str)),
        ) => apply_str_bin_op(l_str, op, r_str),
        (
            FL_T::Primitive(FL_T_Primitive::Float64(l_f64)),
            FL_T::Primitive(FL_T_Primitive::Float64(r_f64)),
        ) => Ok(compute_bin_op_f64(l_f64, op, r_f64)),
        (
            FL_T::Primitive(FL_T_Primitive::UInteger128(l_u128)),
            FL_T::Primitive(FL_T_Primitive::UInteger128(r_u128)),
        ) => Ok(compute_bin_op_u128(l_u128, op, r_u128)),
        _ => todo!("apply_bin_op_to_bin_expr"),
    }
}

// TODO: expand back to generics later probably
fn compute_bin_op(l: i32, op: &BinaryOperator, r: i32) -> FL_T {
    match op {
        BinaryOperator::Minus => FL_T::Primitive(FL_T_Primitive::Integer32(l - r)),
        BinaryOperator::Plus => FL_T::Primitive(FL_T_Primitive::Integer32(l + r)),
        BinaryOperator::Mul => FL_T::Primitive(FL_T_Primitive::Integer32(l * r)),
        BinaryOperator::Div => FL_T::Primitive(FL_T_Primitive::Integer32(l / r)),

        BinaryOperator::Greater => downcast_bin_comparison_to_fl_t_i32(|| l > r),
        BinaryOperator::GreaterEq => downcast_bin_comparison_to_fl_t_i32(|| l >= r),
        BinaryOperator::Less => downcast_bin_comparison_to_fl_t_i32(|| l < r),
        BinaryOperator::LessEq => downcast_bin_comparison_to_fl_t_i32(|| l <= r),
        BinaryOperator::BangEq => downcast_bin_comparison_to_fl_t_i32(|| l != r),
        BinaryOperator::EqEq => downcast_bin_comparison_to_fl_t_i32(|| l == r),
    }
}

fn compute_bin_op_f64(l: f64, op: &BinaryOperator, r: f64) -> FL_T {
    match op {
        BinaryOperator::Minus => FL_T::Primitive(FL_T_Primitive::Float64(l - r)),
        BinaryOperator::Plus => FL_T::Primitive(FL_T_Primitive::Float64(l + r)),
        BinaryOperator::Mul => FL_T::Primitive(FL_T_Primitive::Float64(l * r)),
        BinaryOperator::Div => FL_T::Primitive(FL_T_Primitive::Float64(l / r)),

        BinaryOperator::Greater => downcast_bin_comparison_to_fl_t_i32(|| l > r),
        BinaryOperator::GreaterEq => downcast_bin_comparison_to_fl_t_i32(|| l >= r),
        BinaryOperator::Less => downcast_bin_comparison_to_fl_t_i32(|| l < r),
        BinaryOperator::LessEq => downcast_bin_comparison_to_fl_t_i32(|| l <= r),
        BinaryOperator::BangEq => downcast_bin_comparison_to_fl_t_i32(|| l != r),
        BinaryOperator::EqEq => downcast_bin_comparison_to_fl_t_i32(|| l == r),
    }
}

fn compute_bin_op_u128(l: u128, op: &BinaryOperator, r: u128) -> FL_T {
    match op {
        BinaryOperator::Minus => FL_T::Primitive(FL_T_Primitive::UInteger128(l - r)),
        BinaryOperator::Plus => FL_T::Primitive(FL_T_Primitive::UInteger128(l + r)),
        BinaryOperator::Mul => FL_T::Primitive(FL_T_Primitive::UInteger128(l * r)),
        BinaryOperator::Div => FL_T::Primitive(FL_T_Primitive::UInteger128(l / r)),

        BinaryOperator::Greater => downcast_bin_comparison_to_fl_t_i32(|| l > r),
        BinaryOperator::GreaterEq => downcast_bin_comparison_to_fl_t_i32(|| l >= r),
        BinaryOperator::Less => downcast_bin_comparison_to_fl_t_i32(|| l < r),
        BinaryOperator::LessEq => downcast_bin_comparison_to_fl_t_i32(|| l <= r),
        BinaryOperator::BangEq => downcast_bin_comparison_to_fl_t_i32(|| l != r),
        BinaryOperator::EqEq => downcast_bin_comparison_to_fl_t_i32(|| l == r),
    }
}
fn downcast_bin_comparison_to_fl_t_i32<F>(f: F) -> FL_T
where
    F: Fn() -> bool,
{
    const TRUE_INT_VAL: i32 = 1;
    const FALSE_INT_VAL: i32 = 0;

    if f() {
        FL_T::Primitive(FL_T_Primitive::Integer32(TRUE_INT_VAL))
    } else {
        FL_T::Primitive(FL_T_Primitive::Integer32(FALSE_INT_VAL))
    }
}

pub fn apply_str_bin_op(
    l_str: String,
    op: &BinaryOperator,
    r_str: String,
) -> InterpreterResult<FL_T> {
    match op {
        BinaryOperator::Plus => Ok(FL_T::Primitive(FL_T_Primitive::Str(String::from(
            l_str + &r_str,
        )))),
        // probably not the most optimal way to concat strs because of double conversion but w/e
        _ => Err(format!("Can not apply {:#?} to String and String", op)),
    }
}
pub fn evaluate_native_fn_variant(
    native_fn_variant: Callable_Native,
    env: Rc<RefCell<Environment>>,
) -> InterpreterResult<FL_T> {
    match native_fn_variant {
        Callable_Native::Clock => {
            let start = SystemTime::now();
            let since_the_epoch = start
                .duration_since(UNIX_EPOCH)
                .expect("Time went backwards");
            Ok(FL_T::Primitive(FL_T_Primitive::UInteger128(
                since_the_epoch.as_millis(),
            )))
        }
    }
}

// TODO: Remove this once we have type checks? I'd want my language to be strict and now allow
// stuff like if ("foo") like wtf is a "truthy" value they have played us for absolute fools
pub fn cast_to_bool(x: &FL_T) -> bool {
    match x {
        FL_T::Primitive(FL_T_Primitive::Integer32(int32)) => *int32 > 0,
        FL_T::Primitive(FL_T_Primitive::Str(string)) => string != "",
        FL_T::Primitive(FL_T_Primitive::Bool(flag)) => *flag,
        FL_T::Primitive(FL_T_Primitive::Float64(float64)) => *float64 > 0_f64,
        FL_T::Primitive(FL_T_Primitive::UInteger128(uint128)) => *uint128 > 0_u128,
        FL_T::Primitive(FL_T_Primitive::Nil) => false,
        FL_T::Callable(_) => todo!(),
        FL_T::Unit => false,
    }
}

#[path = "expr.test.rs"]
mod tests;
