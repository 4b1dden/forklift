use crate::grammar::{Declaration, Statement};
use crate::interpreter::{evaluate_expr, evaluate_print_statement, Environment, FL_T};
use crate::parser::LetBinding;

// TODO: change, this is for early dev purposes only
pub type InterpreterResult<T> = Result<T, String>;

pub fn eval_declaration(decl: &Declaration, env: &mut Environment) -> InterpreterResult<FL_T> {
    match decl {
        Declaration::Let(let_binding) => evaluate_let_binding(let_binding, env),
        Declaration::Statement(statement) => evaluate_statement(statement, env),
        Declaration::ScopedBlock(declarations) => {
            let mut local_env = Environment::new(Some(&env));

            let mut last_result = FL_T::Unit;
            for declaration in declarations.iter() {
                last_result = eval_declaration(declaration, &mut local_env)?;
            }

            Ok(last_result)
        }
    }
}

pub fn evaluate_statement(statement: &Statement, env: &mut Environment) -> InterpreterResult<FL_T> {
    match statement {
        Statement::Expr(expr) => evaluate_expr(expr, &env),
        Statement::Print(expr_to_print) => evaluate_print_statement(expr_to_print, &env),
    }
}

pub fn evaluate_let_binding(
    binding: &LetBinding,
    env: &mut Environment,
) -> InterpreterResult<FL_T> {
    let rhs_evaluated = evaluate_expr(&binding.rhs, env)?;

    let _ = env.put(binding.identifier.0.clone(), rhs_evaluated.clone());

    Ok(rhs_evaluated)
}
