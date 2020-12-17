use crate::ast::{exec_block, Exception, FunctionBody};
use crate::interpreter::Interpreter;
use crate::value::Value;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Function {
    Native(FunctionBody),
    Foreign(fn(Vec<Value>) -> Result<Vec<Value>, Exception>),
    // TODO Other callable types (e.g. callable tables?)
}

pub trait Callable {
    fn call(&self, lua: &mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception>;
}

impl Callable for Function {
    fn call(&self, lua: &mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
        match self {
            Function::Native(func) => {
                let FunctionBody(params, block) = func;
                let result = lua.env.activate(|env| {
                    let mut lua = Interpreter::from(env);
                    // TODO make sure params don't depend on other params
                    for (param, arg) in params.iter().zip(&args) {
                        lua.env.set_local(Value::String(param.clone()), arg.clone());
                    }
                    exec_block(&block, &mut lua)
                });
                match result {
                    Ok(()) => Ok(vec![Value::Nil]),
                    Err(Exception::RuntimeError(msg)) => Err(Exception::RuntimeError(msg)),
                    Err(Exception::Return(vals)) => Ok(vals),
                    Err(Exception::UserError(msg)) => Err(Exception::UserError(msg)),
                }
            }
            Function::Foreign(func) => func(args),
        }
    }
}
