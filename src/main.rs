use silica::ast::{exec_block, Exception};
use silica::interpreter::Interpreter;
use silica::lex::scan;
use silica::parser::{parse, Parser};
use std::env;
use std::fs::read_to_string;
use std::io::Error;
use std::process;

fn main() -> Result<(), Error> {
    let filename = if let Some(filename) = parse_args() {
        filename
    } else {
        eprintln!("Incorrect number of arguments");
        process::exit(1);
    };
    let source = read_to_string(filename)?;
    let mut lua = Interpreter::new();
    let mut parser = Parser::new(scan(&source));
    if let Ok(chunk) = parse(&mut parser) {
        // println!("DEBUG {:#?}", chunk);
        if let Err(Exception::RuntimeError(msg)) = exec_block(&chunk, &mut lua) {
            println!("Runtime error: {}", msg);
            process::exit(1);
        }
    } else {
        println!("Syntax error");
        process::exit(1);
    }
    Ok(())
}

fn parse_args() -> Option<String> {
    let mut args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        args.pop()
    } else {
        None
    }
}
