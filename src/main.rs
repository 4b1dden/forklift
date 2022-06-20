use std::env;
use std::fs;
use std::io::{self, BufRead};
use std::path::Path;
use std::process::exit;

mod grammar;
mod parser;
mod tokenizer;

use crate::grammar::parse_declaration;
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
    let parsed_program = program_parser.parse(&contents);

    println!("{:#?}", parsed_program);
}

fn run_repl_loop() {
    let mut line = String::new();
    let stdin = io::stdin();

    loop {
        println!("fl >>>");
        line.clear();
        stdin.lock().read_line(&mut line).unwrap();

        if line.trim().to_lowercase() != "exit" {
            let dec = parse_declaration().parse(&line);
            println!("{:#?}", dec);
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
