use crate::{
    grammar::{Declaration, Program, Statement},
    interpreter::desugar_for_loop_to_while_block,
};

pub fn optimize_program(program: Program) -> Program {
    let desugared_program = desugar_program(program);

    // other optimizations ...
    desugared_program
}

pub enum DesugarResult {
    Terminal,
    Optimized { declaration: Declaration },
}

pub fn desugar_program(program: Program) -> Program {
    let mut desugared = vec![];

    for old_declaration in program.into_iter() {
        match desugar_declaration(&old_declaration) {
            DesugarResult::Terminal => {
                desugared.push(old_declaration);
            }
            DesugarResult::Optimized { declaration } => {
                desugared.push(declaration);
            }
        }
    }

    desugared
}

pub fn desugar_declaration(dec: &Declaration) -> DesugarResult {
    match dec {
        Declaration::Let(_) => DesugarResult::Terminal,
        Declaration::Reassignment(_) => DesugarResult::Terminal, // todo: maybe allow i++ => i = i + 1;
        Declaration::Statement(statement) => desugar_statement(statement),
    }
}

pub fn desugar_statement(stmt: &Statement) -> DesugarResult {
    match stmt {
        Statement::ForLoop(for_loop_def) => {
            let desugared = desugar_for_loop_to_while_block(for_loop_def)
                .expect("Could not desugar for loop to while loop oops");

            DesugarResult::Optimized {
                declaration: Declaration::Statement(desugared),
            }
        }
        _ => DesugarResult::Terminal,
    }
}
