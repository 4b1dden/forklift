use std::env;
use std::fs;
use std::io::{self, BufRead};
use std::path::Path;
use std::process::exit;

mod grammar;
mod interpreter;
mod parser;

use crate::grammar::parse_declaration;
use crate::interpreter::{Environment, Interpreter};
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
    } else {
        println!("Unknown mode, use repl | load");
        exit(1);
    }
}

fn load_and_eval_file(path: &Path) {
    let contents = fs::read_to_string(path).expect("Filepath has to be valid");

    let program_parser = grammar::parse_program();
    let (rest, parsed_program) = program_parser.parse(&contents).unwrap();

    println!("{:#?}", parsed_program);

    let dec_count = parsed_program.len();
    let mut interpreter = Interpreter::new(parsed_program);
    let res = interpreter.load_defaults();

    println!("[FL]: ------ PROGRAM EVALUATION START");
    let executed_prog = interpreter.interpret_program();
    println!("[FL]: ------ PROGRAM EVALUATION END");

    println!("[FL]: Successfully executed program!");
    println!("[FL]: Declarations: {:#?}", dec_count);
    println!("[FL]: Dump of global_env: {:#?}", interpreter.global_env);

    // println!("{:#?}", parsed_program);
}

fn run_repl_loop() {
    let mut line = String::new();
    let stdin = io::stdin();
    let parser = grammar::parse_program();
    let mut repl_env = Environment::new(None);

    loop {
        println!("fl >>>");
        line.clear();
        stdin.lock().read_line(&mut line).unwrap();

        if line.trim().to_lowercase() != "exit" {
            let (rest, dec) = grammar::parse_declaration(&line).unwrap();
            let evaled = interpreter::declaration::eval_declaration(&dec, &mut repl_env);
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
