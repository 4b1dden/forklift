use crate::vm::scanner::{tokenize, LexedToken, Lexer, LexerError, TokenKind};
use crate::vm::{
    Chunk, OpCode, Value, OP_ADD, OP_CONSTANT, OP_DIVIDE, OP_NEGATE, OP_RETURN, OP_SUBTRACT,
};

pub type CompilerResult<T> = Result<T, String>; // Change Err type later

struct Compiler<'a> {
    had_error: bool,
    panic_mode: bool,
    tokenizer: Lexer<'a>,
}

pub fn compile(source: &str) -> CompilerResult<Chunk> {
    let mut tokenizer = Lexer::new(source);
    let mut tokens = vec![];
    let mut compiling_chunk = Chunk::new();

    if let Ok(tok) = tokenizer.next_token() {
        tokens.push(tok);
    } else {
    }

    expression(&mut tokenizer);
    // consume(TokenKind::EOF, "Expected EOF");

    // let lexed_tokens = tokenize(source).expect("Tokenizer should not fail");

    emit_return(&mut compiling_chunk);

    if tokenizer.had_error {
        Err(String::from("foooo!"))
    } else {
        Ok(Chunk::new())
    }
}

impl<'a> Compiler<'a> {
    fn make_error_at_lexeme(&self, tok: LexedToken, msg: &str) -> CompilerResult<()> {
        if !self.panic_mode {
            Err(format!(
                "Error at {}, token type: {:?}; \n Message: {}",
                tok.index_start, tok.kind, msg
            ))
        } else {
            Ok(())
        }
    }
}

fn expression(tokenizer: &mut Lexer) {}

fn number(tokenizer: &mut Lexer) {
    //let val = tokenizer.previous.clone();
}

fn emit_return(chunk: &mut Chunk) {
    write_chunk(chunk, OP_RETURN)
}

fn write_chunk(chunk: &mut Chunk, byte: u8) {
    chunk.write(byte, 123)
}

fn make_constant(chunk: &mut Chunk, val: Value) -> CompilerResult<OpCode> {
    let const_idx = chunk.add_constant(val);

    if const_idx > u8::MAX {
        return Err(String::from("TOO MANY CONSTS IN CHUNK"));
    }

    Ok(const_idx)
}

fn write_constant(chunk: &mut Chunk, val: Value) -> CompilerResult<()> {
    let idx = make_constant(chunk, val)?;

    write_chunk_byte_tuple(chunk, OP_CONSTANT, idx);

    Ok(())
}

fn write_chunk_byte_tuple(chunk: &mut Chunk, byte1: u8, byte2: u8) {
    write_chunk(chunk, byte1);
    write_chunk(chunk, byte2);
}

fn consume(tokenizer: &mut Lexer, kind: TokenKind) -> CompilerResult<()> {
    tokenizer.consume(kind)
}
