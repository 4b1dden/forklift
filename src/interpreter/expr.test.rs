use std::cell::RefCell;
use std::rc::Rc;

use super::{desugar_for_loop_to_while_block, FL_T_Primitive, FL_T};
use crate::grammar::{Declaration, Statement};
use crate::interpreter::{Environment, Interpreter, InterpreterResult};
use crate::parser::{
    BinaryExpr, BinaryOperator, Expr, ForLoop, Identifier, LetBinding, LiteralExpr, Number,
    Reassignment, StringLiteral, UnaryExpr, UnaryOperator,
};

fn empty_env() -> Rc<RefCell<Environment>> {
    Rc::new(RefCell::new(Environment::new(None)))
}

fn interpreter() -> Interpreter {
    Interpreter::new(vec![])
}

#[test]
fn test_literal_expr() {
    let to_intepret = Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(3)));

    assert_eq!(
        interpreter()
            .interpreter()
            .evaluate_expr(&to_intepret, empty_env()),
        Ok(FL_T::Primitive(FL_T_Primitive::Integer32(3)))
    );
}

#[test]
fn test_unary_expr() {
    let inner = Expr::Unary(UnaryExpr {
        op: UnaryOperator::Minus,
        expr: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
            Number::Integer32(5),
        ))),
    });

    let to_intepret = inner.clone();
    let nested = Expr::Unary(UnaryExpr {
        op: UnaryOperator::Minus,
        expr: Box::new(inner.clone()),
    });

    assert_eq!(
        interpreter().evaluate_expr(&to_intepret, empty_env()),
        Ok(FL_T::Primitive(FL_T_Primitive::Integer32(-5)))
    );

    assert_eq!(
        interpreter().evaluate_expr(&nested, empty_env()),
        Ok(FL_T::Primitive(FL_T_Primitive::Integer32(5)))
    );
}

#[test]
fn test_binary_expr() {
    // TODO: extend test cases
    let inner = Expr::Binary(BinaryExpr {
        lhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
            Number::Integer32(5),
        ))),
        rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
            Number::Integer32(10),
        ))),
        op: BinaryOperator::Mul,
    });

    let nested = Expr::Binary(BinaryExpr {
        lhs: Box::new(inner.clone()),
        rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
            Number::Integer32(3),
        ))),
        op: BinaryOperator::Plus,
    });

    assert_eq!(
        interpreter().evaluate_expr(&inner, empty_env()),
        Ok(FL_T::Primitive(FL_T_Primitive::Integer32(50)))
    );

    assert_eq!(
        interpreter().evaluate_expr(&nested, empty_env()),
        Ok(FL_T::Primitive(FL_T_Primitive::Integer32(53)))
    );
}

#[test]
fn test_desugar_for_loop() {
    // for (let k = 0; k < 10; k = k + 1) { print a; }
    let for_loop = ForLoop {
        init_declaration: Declaration::Let(LetBinding {
            identifier: Identifier(String::from("k")),
            rhs: Expr::Literal(LiteralExpr::NumberLiteral(Number::Integer32(0))),
        }),
        condition: Expr::Binary(BinaryExpr {
            lhs: Box::new(Expr::Literal(LiteralExpr::Identifier(Identifier(
                String::from("k"),
            )))),
            op: BinaryOperator::Less,
            rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                Number::Integer32(10),
            ))),
        }),
        post_iteration: Declaration::Reassignment(Reassignment {
            identifier: Identifier(String::from("k")),
            rhs: Expr::Binary(BinaryExpr {
                lhs: Box::new(Expr::Literal(LiteralExpr::Identifier(Identifier(
                    String::from("k"),
                )))),
                op: BinaryOperator::Plus,
                rhs: Box::new(Expr::Literal(LiteralExpr::NumberLiteral(
                    Number::Integer32(1),
                ))),
            }),
        }),
        body: Statement::Block(vec![Declaration::Statement(Statement::Print(
            Expr::Literal(LiteralExpr::Identifier(Identifier(String::from("a")))),
        ))]),
    };

    let desugared = desugar_for_loop_to_while_block(&for_loop);

    println!("{:#?}", desugared);
}
