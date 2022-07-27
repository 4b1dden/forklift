use crate::vm::compile;
use std::{
    fs,
    io::{self, BufRead},
    path::Path,
    process::exit,
};

pub type OpCode = u8;

pub const OP_RETURN: OpCode = 0x0;
pub const OP_CONSTANT: OpCode = 0x1;
pub const OP_NEGATE: OpCode = 0x2;

pub const OP_ADD: OpCode = 0x3;
pub const OP_SUBTRACT: OpCode = 0x4;
pub const OP_MULTIPLY: OpCode = 0x5;
pub const OP_DIVIDE: OpCode = 0x6;

fn primitive_instruction(name: &str, offset: usize) -> usize {
    print!("{:#}", name);
    print!("\n");

    offset + 1
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let const_idx = chunk.code[offset + 1] as u8;

    print!("{:#} {:?} ", name, const_idx);
    print_value(chunk.constants.values[const_idx as usize]);
    print!("\n");

    offset + 2
}

pub type Value = f64;

pub fn print_value(val: Value) {
    print!("       {}", val);
}

pub struct ValueArray {
    count: usize,
    capacity: usize,
    values: Vec<Value>,
}

const CAPACITY_LOW_THRESHOLD: usize = 8;
const CAPACITY_INCREMENT_MULTIPLIER: usize = 2;
const STACK_CAPACITY: usize = 256;

#[derive(Debug)]
pub enum INTERPRETER_ERROR {
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
}

pub type VmResult<T> = Result<T, INTERPRETER_ERROR>;

const DEBUG_TRACE_EXECUTION: bool = true;

pub struct VM {
    chunk: Chunk,
    ip: u8,
    stack: Vec<Value>, // SIZE = STACK_CAPACITY
}

impl VM {
    pub fn interpret(chunk: Chunk) -> VmResult<()> {
        let mut vm = Self::with_chunk(chunk);

        vm.run()
    }

    pub fn with_chunk(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::with_capacity(STACK_CAPACITY),
        }
    }

    pub fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    pub fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn run(&mut self) -> VmResult<()> {
        let result = loop {
            let byte = read_byte(self);

            if DEBUG_TRACE_EXECUTION {
                print!("        ");
                for val in self.stack.iter() {
                    print!("[ {} ]", val);
                }
                print!("\n");

                disassemble_instruction(&self.chunk, (self.ip - 1) as usize);
            }

            match byte {
                OP_CONSTANT => {
                    let const_val = read_const(self);
                    self.push(const_val);
                }
                OP_RETURN => {
                    print_value(self.pop());
                    println!();

                    break Ok(());
                }
                OP_NEGATE => {
                    let popped = self.pop();
                    self.push(-popped);
                }
                OP_ADD => {
                    let (b, a) = (self.pop(), self.pop());
                    self.push(a + b);
                }
                OP_SUBTRACT => {
                    let (b, a) = (self.pop(), self.pop());
                    self.push(a - b);
                }
                OP_MULTIPLY => {
                    let (b, a) = (self.pop(), self.pop());
                    self.push(b * a);
                }
                OP_DIVIDE => {
                    let (b, a) = (self.pop(), self.pop());
                    self.push(a / b);
                }
                _ => panic!("Unknown instruction {}", byte),
            }
        };

        result
    }
}

fn read_byte(vm: &mut VM) -> u8 {
    let byte = vm.chunk.code[vm.ip as usize];

    vm.ip = vm.ip + 1;

    byte
}

fn read_const(vm: &mut VM) -> Value {
    let b = read_byte(vm);

    vm.chunk.constants.values[b as usize]
}

impl ValueArray {
    pub fn new() -> Self {
        Self {
            count: 0,
            capacity: 0,
            values: Vec::new(),
        }
    }

    fn upgrade_capacity(&mut self) {
        if self.capacity < CAPACITY_LOW_THRESHOLD {
            self.capacity = CAPACITY_LOW_THRESHOLD;
        } else {
            self.capacity = self.capacity * CAPACITY_INCREMENT_MULTIPLIER;
        }
    }

    pub fn write(&mut self, val: Value) {
        if self.capacity < self.count + 1 {
            self.upgrade_capacity();
        }

        self.values.push(val);
        self.count = self.count + 1;

        assert!(self.count <= self.capacity);
    }
}

pub struct Chunk {
    count: usize,
    capacity: usize,
    code: Vec<OpCode>,
    constants: ValueArray,
    lines: Vec<u32>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            count: 0,
            capacity: 0,
            code: Vec::new(),
            constants: ValueArray::new(),
            lines: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, val: Value) -> u8 {
        self.constants.write(val);

        self.constants.count as u8 - 1_u8 // idx of added const
    }

    pub fn write(&mut self, byte: OpCode, line: u32) {
        if self.capacity < self.count + 1 {
            self.upgrade_capacity();
        }

        self.code.push(byte);
        self.count = self.count + 1;
        self.lines.push(line);

        assert!(self.count <= self.capacity);
    }

    // 0 -> 1 -> 2 -> 4 -> 8 -> ...
    fn upgrade_capacity(&mut self) {
        if self.capacity < CAPACITY_LOW_THRESHOLD {
            self.capacity = CAPACITY_LOW_THRESHOLD;
        } else {
            self.capacity = self.capacity * CAPACITY_INCREMENT_MULTIPLIER;
        }
    }

    pub fn disassemble(&self, name: &str) {
        println!("=== {:#} ===", name);

        let mut offset = 0;
        while offset < self.count {
            offset = disassemble_instruction(&self, offset);
        }

        println!(" ------- op sourcecode ");
        println!("{:?}", self.code);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{} ", offset);

    if offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1] {
        print!("     |  ");
    } else {
        print!("   {}  ", chunk.lines[offset]);
    }

    let instruction = &chunk.code[offset];

    match *instruction {
        OP_RETURN => primitive_instruction("RETURN", offset),
        OP_CONSTANT => constant_instruction("CONSTANT", chunk, offset),
        OP_NEGATE => primitive_instruction("NEGATE", offset),
        OP_ADD => primitive_instruction("ADD", offset),
        OP_SUBTRACT => primitive_instruction("SUBTRACT", offset),
        OP_MULTIPLY => primitive_instruction("MULTIPLY", offset),
        OP_DIVIDE => primitive_instruction("DIVIDE", offset),
        _ => todo!("disassemble_instruction not exhaustive"),
    }
}

pub fn main(args: &[String]) {
    if args[2] == "repl" {
        run_repl_loop();
    } else if args[2] == "load" {
        let path = Path::new(&args[3]);
        load_and_eval_file(path);
    } else {
        println!("Unknown mode, use repl | load");
        exit(0);
    }
}

fn interpret(source: &str) -> VmResult<()> {
    let compiled_chunk = compile(source).map_err(|_| INTERPRETER_ERROR::INTERPRET_COMPILE_ERROR)?;

    let mut vm = VM::with_chunk(compiled_chunk);

    vm.run()
}

fn load_and_eval_file(path: &Path) {
    let contents = fs::read_to_string(path).expect("Filepath has to be valid");
}

use crate::vm::tokenize;
fn run_repl_loop() {
    let mut line = String::new();
    let stdin = io::stdin();

    loop {
        println!("fl >>>");
        line.clear();
        stdin.lock().read_line(&mut line).unwrap();

        if line.trim().to_lowercase() != "exit" {
            let result = tokenize(&line);
            println!("{:?}", result);
        } else {
            break;
        }
    }
}
