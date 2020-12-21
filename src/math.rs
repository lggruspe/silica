use crate::ast::Exception;
use crate::env::Environment;
use crate::function::Function;
use crate::interpreter::Interpreter;
use crate::object::{Object, ObjectReference};
use crate::table::Table;
use crate::value::{Float, Value};

// TODO check casts

fn abs(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => {
            if let Some(m) = n.checked_abs() {
                Ok(vec![Value::Integer(m)])
            } else {
                Ok(vec![Value::Integer(i64::MIN)])
            }
        }
        Some(Value::Float(Float(x))) => Ok(vec![Value::Float(Float(x.abs()))]),
        _ => Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        ))),
    }
}

fn acos(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => {
            let n = *n;
            if n < -1 || n > 1 {
                Ok(vec![Value::Float(Float(f64::NAN))])
            } else {
                let x = n as f64;
                Ok(vec![Value::Float(Float(x.acos()))])
            }
        }
        Some(Value::Float(Float(x))) => Ok(vec![Value::Float(Float(x.acos()))]),
        _ => Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        ))),
    }
}

fn asin(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => {
            let n = *n;
            if n < -1 || n > 1 {
                Ok(vec![Value::Float(Float(f64::NAN))])
            } else {
                let x = n as f64;
                Ok(vec![Value::Float(Float(x.asin()))])
            }
        }
        Some(Value::Float(Float(x))) => Ok(vec![Value::Float(Float(x.asin()))]),
        _ => Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        ))),
    }
}

// TODO atan

fn ceil(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => Ok(vec![Value::Integer(*n)]),
        Some(Value::Float(Float(x))) => Ok(vec![Value::Float(Float(x.ceil()))]),
        _ => Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        ))),
    }
}

fn cos(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => {
            let x = *n as f64;
            Ok(vec![Value::Float(Float(x.cos()))])
        }
        Some(Value::Float(Float(x))) => Ok(vec![Value::Float(Float(x.cos()))]),
        _ => Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        ))),
    }
}

fn deg(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => {
            let x = *n as f64;
            Ok(vec![Value::Float(Float(x.to_degrees()))])
        }
        Some(Value::Float(Float(x))) => Ok(vec![Value::Float(Float(x.to_degrees()))]),
        _ => Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        ))),
    }
}

fn exp(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => {
            let x = *n as f64;
            Ok(vec![Value::Float(Float(x.exp()))])
        }
        Some(Value::Float(Float(x))) => Ok(vec![Value::Float(Float(x.exp()))]),
        _ => Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        ))),
    }
}

fn floor(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => Ok(vec![Value::Integer(*n)]),
        Some(Value::Float(Float(x))) => Ok(vec![Value::Float(Float(x.floor()))]),
        _ => Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        ))),
    }
}

// TODO fmod
// TODO log(x [, base])

fn max(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    if args.is_empty() {
        Err(Exception::UserError(Value::String(
            "bad argument #1 (value expected)".to_string(),
        )))
    } else {
        let mut max = args.first().unwrap().clone();
        for arg in args.iter().skip(1) {
            if max.is_lt(&arg) {
                max = arg.clone();
            }
        }
        Ok(vec![max])
    }
}

fn min(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    if args.is_empty() {
        Err(Exception::UserError(Value::String(
            "bad argument #1 (value expected)".to_string(),
        )))
    } else {
        let mut min = args.first().unwrap().clone();
        for arg in args.iter().skip(1) {
            if arg.is_lt(&min) {
                min = arg.clone();
            }
        }
        Ok(vec![min])
    }
}

// TODO modf, random, randomseed

fn sin(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => {
            let x = *n as f64;
            Ok(vec![Value::Float(Float(x.sin()))])
        }
        Some(Value::Float(Float(x))) => Ok(vec![Value::Float(Float(x.sin()))]),
        _ => Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        ))),
    }
}

fn tan(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => {
            let x = *n as f64;
            Ok(vec![Value::Float(Float(x.tan()))])
        }
        Some(Value::Float(Float(x))) => Ok(vec![Value::Float(Float(x.tan()))]),
        _ => Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        ))),
    }
}

fn tointeger(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => Ok(vec![Value::Integer(*n)]),
        Some(Value::String(_)) => unimplemented!(),
        Some(Value::Float(_)) => unimplemented!(),
        _ => Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        ))),
    }
}

fn type_(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(_)) => Ok(vec![Value::String("integer".to_string())]),
        Some(Value::Float(_)) => Ok(vec![Value::String("float".to_string())]),
        Some(_) => Ok(vec![Value::Nil]),
        None => Err(Exception::UserError(Value::String(
            "bad argument #1 (value expected)".to_string(),
        ))),
    }
}

// TODO ult

fn function_object(func: fn(*mut Interpreter, Vec<Value>) -> Result<Vec<Value>, Exception>) -> Value {
    Value::Reference(ObjectReference::new(Object::Function(Function::Foreign(
        func,
    ))))
}

pub fn import_into(env: &mut Environment) {
    let mut math = Table::new();
    let _ = math.set(Value::String("abs".to_string()), function_object(abs));
    let _ = math.set(Value::String("acos".to_string()), function_object(acos));
    let _ = math.set(Value::String("asin".to_string()), function_object(asin));
    let _ = math.set(Value::String("ceil".to_string()), function_object(ceil));
    let _ = math.set(Value::String("cos".to_string()), function_object(cos));
    let _ = math.set(Value::String("deg".to_string()), function_object(deg));
    let _ = math.set(Value::String("exp".to_string()), function_object(exp));
    let _ = math.set(Value::String("floor".to_string()), function_object(floor));
    let _ = math.set(Value::String("max".to_string()), function_object(max));
    let _ = math.set(Value::String("min".to_string()), function_object(min));
    let _ = math.set(Value::String("sin".to_string()), function_object(sin));
    let _ = math.set(Value::String("tan".to_string()), function_object(tan));
    let _ = math.set(
        Value::String("tointeger".to_string()),
        function_object(tointeger),
    );
    let _ = math.set(Value::String("type".to_string()), function_object(type_));

    // Constants
    let _ = math.set(
        Value::String("huge".to_string()),
        Value::Float(Float(f64::INFINITY)),
    );
    let _ = math.set(
        Value::String("maxinteger".to_string()),
        Value::Integer(i64::MAX),
    );
    let _ = math.set(
        Value::String("mininteger".to_string()),
        Value::Integer(i64::MIN),
    );
    let _ = math.set(
        Value::String("pi".to_string()),
        Value::Float(Float(std::f64::consts::PI)),
    );
    env.set(
        Value::String("math".to_string()),
        Value::Reference(ObjectReference::new(Object::Table(math))),
    );
}
