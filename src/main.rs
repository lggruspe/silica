use silica::interpreter::{exec_block, Exception, Interpreter};
use silica::parser::{parse, SyntaxError};
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
    match parse(&source) {
        Ok(chunk) => {
            // eprintln!("DEBUG {:#?}", chunk);
            match exec_block(&chunk, &mut lua) {
                Err(Exception::RuntimeError(msg)) => {
                    eprintln!("Runtime error: {}", msg);
                    process::exit(1);
                }
                Err(Exception::UserError(msg)) => {
                    eprintln!("Runtime error: {:?}", msg);
                    process::exit(1);
                }
                Err(_) => unimplemented!(),
                Ok(()) => (),
            }
        }
        Err(SyntaxError(msg)) => {
            eprintln!("Syntax error: {}", msg);
            process::exit(1);
        }
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
