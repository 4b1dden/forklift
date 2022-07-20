use std::fs::{self, File};

use crate::grammar;
use crate::interpreter::{Interpreter, Resolver};
use crate::parser::Parser;

// maybe should use builtin Path, this will do for the mvp for now
fn with_folder(path: &str) -> String {
    "test/".to_string() + path
}

fn run_stdout_test_case(source_file_name: &str) {
    let path = with_folder(&(source_file_name.to_string() + ".test.fl"));

    let source = fs::read_to_string(path).expect(&format!(
        "Test file {:?}.test.fl does not exist. Quitting test case.",
        source_file_name
    ));

    let program_parser = grammar::parse_program();
    let (rest, parsed_program) = program_parser
        .parse(&source)
        .expect("Could not successfully finish parsing");

    if rest != "" {
        panic!(
            "Although parser did not throw an error, it did not parse the entire file. Quitting"
        );
    }

    let expected_out_file_name = with_folder(&(source_file_name.to_string() + ".out"));
    let expected_out_content = fs::read_to_string(expected_out_file_name)
        .expect("Test output file {:?}.out does not exist. Quitting test case");

    // execute test program

    let output_file_stream = File::create(with_folder(&(source_file_name.to_string() + ".temp")))
        .expect("Can not create a new file");
    let interpreter = Interpreter::new(parsed_program.clone(), output_file_stream);

    let mut resolver = Resolver::new(interpreter);
    resolver
        .resolve_program(parsed_program)
        .expect("An error during resolution");

    let interpreter_result = resolver.interpreter.interpret_program();

    if let Err(interpreter_err) = interpreter_result {
        panic!(
            "Test case #[{:#}] interpreter error: {:#?}",
            source_file_name.to_string(),
            interpreter_err
        );
    }

    // TODO: we should not use a temp file but a string that is std::io::Write
    // load temp file
    let temp_file_name = with_folder(&(source_file_name.to_string() + ".temp"));
    let executed_prog_output =
        fs::read_to_string(temp_file_name.clone()).expect(".temp file was written");

    // cleanup temp file
    fs::remove_file(temp_file_name).expect(&format!(
        "Could not delete temp file for #[{:#}]",
        source_file_name
    ));

    assert_eq!(expected_out_content, executed_prog_output);
}

#[test]
fn test_stdout_print() {
    run_stdout_test_case("print_stdout")
}

#[test]
fn test_variables() {
    run_stdout_test_case("variables")
}

#[test]
fn test_if_else() {
    run_stdout_test_case("if_else")
}

/*
impl Write for StringBuffer {
fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {

}}
*/
