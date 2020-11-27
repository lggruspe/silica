use crate::ast::{exec_block, Exception, FunctionBody};
use crate::interpreter::Interpreter;
use crate::table::Table;
use crate::value::Value;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Function {
    Native(FunctionBody),
    Foreign(fn(Vec<Value>) -> Result<Vec<Value>, Exception>),
    // TODO Other callable types (e.g. callable tables?)
}

pub trait Callable {
    fn call(&mut self, lua: &mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception>;
}

impl Callable for Function {
    fn call(&mut self, lua: &mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
        match self {
            Function::Native(func) => {
                let FunctionBody(params, block) = func;
                lua.env.locals.push(Table::new());
                // TODO make sure params don't depend on other params
                for (param, arg) in params.iter().zip(args) {
                    lua.env.set_local(Value::String(param.clone()), arg);
                }
                let result = exec_block(&block, lua);
                lua.env.locals.pop();
                match result {
                    Ok(()) => Ok(vec![Value::Nil]),
                    Err(Exception::RuntimeError(msg)) => Err(Exception::RuntimeError(msg)),
                    Err(Exception::Return(vals)) => {
                        if let Some(val) = vals.first() {
                            Ok(vec![val.clone()])
                        } else {
                            Ok(vec![Value::Nil])
                        }
                        // TODO multiple return values
                    }
                }
            }
            Function::Foreign(func) => func(args),
        }
    }
}

pub fn print_(args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    // TODO
    if let Some(arg) = args.first() {
        print!("{}", arg.tostring());
    }
    for arg in args.iter().skip(1) {
        print!("\t{}", arg.tostring());
    }
    println!("");
    Ok(vec![Value::Nil])
}