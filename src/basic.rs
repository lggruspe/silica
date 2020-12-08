use crate::ast::Exception;
use crate::value::{Float, Value};

pub fn assert(args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    let val = args.first().unwrap_or(&Value::Nil);
    if val.is_truthy() {
        Ok(vec![val.clone()])
    } else {
        let msg = if let Some(msg) = args.get(1) {
            msg.clone()
        } else {
            Value::String("assertion failed!".to_string())
        };
        Err(Exception::UserError(msg))
    }
}

pub fn error(args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    // TODO error(message, [level]): use level
    match args.first() {
        Some(Value::Integer(n)) => Err(Exception::UserError(Value::Integer(*n))),
        Some(Value::Float(Float(x))) => Err(Exception::UserError(Value::Float(Float(*x)))),
        Some(Value::String(s)) => Err(Exception::UserError(Value::String(s.clone()))),
        Some(v) => Err(Exception::UserError(Value::String(format!(
            "error object is a {} value",
            v.type_str()
        )))),
        None => Err(Exception::UserError(Value::String(
            "error object is a nil value".to_string(),
        ))),
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

pub fn tonumber(args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    // TODO tonumber(e [, base])
    match args.first() {
        Some(Value::Integer(n)) => Ok(vec![Value::Integer(*n)]),
        Some(Value::Float(Float(x))) => Ok(vec![Value::Float(Float(*x))]),
        Some(Value::String(s)) => {
            if let Ok(n) = s.parse::<i64>() {
                Ok(vec![Value::Integer(n)])
            } else if let Ok(x) = s.parse::<f64>() {
                Ok(vec![Value::Float(Float(x))])
            } else {
                Ok(vec![Value::Nil])
            }
        }
        _ => Ok(vec![Value::Nil]),
    }
}

pub fn tostring(args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    // TODO use __tostring or __name metamethods
    if let Some(v) = args.first() {
        Ok(vec![Value::String(v.tostring())])
    } else {
        Ok(vec![Value::String(Value::Nil.tostring())])
    }
}

pub fn type_(args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    if let Some(v) = args.first() {
        Ok(vec![Value::String(v.type_str().to_string())])
    } else {
        Ok(vec![Value::String(Value::Nil.type_str().to_string())])
    }
}

// TODO ipairs, next, pairs, warn
