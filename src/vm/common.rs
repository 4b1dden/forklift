pub type OpCode = u8;

pub const OP_RETURN: OpCode = 0x0;
pub const OP_CONSTANT: OpCode = 0x1;

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

pub struct VM {
    chunk: Chunk,
    ip: u8,
}

#[derive(Debug)]
pub enum INTERPRETER_RESULT {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
}

impl VM {
    pub fn interpret(chunk: Chunk) -> INTERPRETER_RESULT {
        let mut vm = Self::with_chunk(chunk);

        vm.run()
    }

    pub fn with_chunk(chunk: Chunk) -> Self {
        Self { chunk, ip: 0 }
    }

    fn run(&mut self) -> INTERPRETER_RESULT {
        let result = loop {
            let byte = read_byte(self);

            if byte == OP_CONSTANT {
                let const_val = read_const(self);
                print_value(const_val);
                print!("\n");
            } else if byte == OP_RETURN {
                break INTERPRETER_RESULT::INTERPRET_OK;
            } else {
                break INTERPRETER_RESULT::INTERPRET_COMPILE_ERROR;
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
            offset = Chunk::disassemble_instruction(&self, offset);
        }

        println!(" ------- op sourcecode ");
        println!("{:?}", self.code);
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
            _ => todo!("disassemble_instruction not exhaustive"),
        }
    }
}

pub fn main() {
    let mut chunk = Chunk::new();

    let constant1 = chunk.add_constant(1.2);
    chunk.write(OP_CONSTANT, 123);
    chunk.write(constant1, 123);

    let constant2 = chunk.add_constant(2.4);
    chunk.write(OP_CONSTANT, 123);
    chunk.write(constant2, 123);

    chunk.write(OP_RETURN, 456);

    // chunk.disassemble("test");
    let result = VM::interpret(chunk);
    println!("{:?}", result);
}
