use std::cell::RefCell;
use std::env;
use std::fs;
use std::fs::File;
use std::io::BufWriter;
use std::io::{self, BufRead};
use std::path::Path;
use std::process::exit;
use std::rc::Rc;

mod grammar;
mod interpreter;
mod optimizer;
mod parser;
mod vm;

#[cfg(test)]
#[path = "../test/mod.rs"]
mod e2e_tests;

use crate::grammar::parse_declaration;
use crate::interpreter::{resolver::Resolver, Environment, Interpreter};
use crate::parser::{BoxedParser, Expr, Parser};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_help();
        exit(1);
    }

    let mode = &args[1];
    if mode == "repl" {
        run_repl_loop();
    } else if mode == "load" {
        let filepath = Path::new(&args[2]);
        load_and_eval_file(filepath);
    } else if mode == "vm" {
        vm::main(&args);
    } else {
        println!("Unknown mode {:#}, use repl | load", mode);
        exit(1);
    }
}

fn load_and_eval_file(path: &Path) {
    let contents = fs::read_to_string(path).expect("Filepath has to be valid");

    let program_parser = grammar::parse_program();
    let (rest, parsed_program) = program_parser.parse(&contents).unwrap();

    let optimized_program = optimizer::optimize_program(parsed_program);

    if rest != "" {
        println!("[FL]: ------ ERROR IN PARSER, EXITING");
        println!("rest: {:#?}", rest);
        exit(1);
    }

    println!("{:#?}", optimized_program);

    let dec_count = optimized_program.len();
    //let file_stream = File::create("foo.flout").expect("The file should exist");
    let stdout_stream = io::stdout();
    let interpreter = Interpreter::new(optimized_program.clone(), stdout_stream);

    let mut resolver = Resolver::new(interpreter);

    let resolved = resolver.resolve_program(optimized_program);

    if let Err(resolution_err) = resolved {
        println!("[FL]: ------ ERROR IN RESOLVER, EXITING");
        println!("{:#?}", resolution_err);
        exit(1);
    }

    println!("[FL]: ------ FINISHED PROGRAM RESOLUTION");
    println!("{:#?}", &resolver.interpreter.locals);

    println!("[FL]: ------ PROGRAM EVALUATION START");
    let executed_prog = resolver.interpreter.interpret_program();

    println!("[FL]: ------ PROGRAM EVALUATION END");

    println!("[FL]: Successfully executed program!");
    println!("[FL]: {:#?}", executed_prog);
    println!("[FL]: Declarations: {:#?}", dec_count);
}

fn run_repl_loop() {
    let mut line = String::new();
    let stdin = io::stdin();
    let parser = grammar::parse_program();
    let interpreter = Interpreter::new(vec![], io::stdout());
    let mut repl_env = Rc::new(RefCell::new(Environment::new(None)));

    loop {
        println!("fl >>>");
        line.clear();
        stdin.lock().read_line(&mut line).unwrap();

        if line.trim().to_lowercase() != "exit" {
            let (rest, dec) = grammar::parse_declaration(&line).unwrap();
            let evaled = interpreter.eval_declaration(&dec, repl_env.clone());
            println!("evaluated ---> {:#?} \n rest ---> {:#?}", evaled, rest);
        } else {
            break;
        }
    }
}

fn print_help() {
    println!(
        "
        Usage: 
            [0] [required]          repl | load
            [1] [if --mode==load]   filepath
    "
    );
}
