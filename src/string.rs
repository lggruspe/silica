use crate::ast::Exception;
use crate::env::Environment;
use crate::function::Function;
use crate::interpreter::Interpreter;
use crate::object::{Object, ObjectReference};
use crate::table::Table;
use crate::value::{Float, Value};

fn byte(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    let string = match args.first() {
        Some(Value::Integer(n)) => n.to_string(),
        Some(Value::Float(Float(x))) => x.to_string(),
        Some(Value::String(s)) => s.clone(),
        _ => return Err(Exception::UserError(Value::String("bad argument #1 (string expected)".to_string()))),
    };
    let start = if let Value::Integer(n) = args.get(1).unwrap_or(&Value::Integer(1)).to_integer() {
        n
    } else {
        return Err(Exception::UserError(Value::String("bad argument #2 (number expected)".to_string())));
    };
    let n = string.len() as i64;
    let end = match args.get(2) {
        None => start,
        Some(Value::Nil) => start,
        Some(val) => {
            if let Value::Integer(n) = val.to_integer() {
                n
            } else {
                return Err(Exception::UserError(Value::String("bad argument #3 (number expected)".to_string())));
            }
        }
    };
    let start = if start < -n {
        n
    } else if start < 0 {
        n + start
    } else if start == 0 {
        0
    } else if start < n {
        start - 1
    } else {
        n
    } as usize;
    let end = if end < -n {
        n
    } else if end < 0 {
        n + end + 1
    } else if end == 0 {
        0
    } else if end < n {
        end
    } else {
        n
    } as usize;
    Ok(vec![Value::String(String::from(&string[start..end]))])
}

fn char_(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    let n = args.len();
    let mut string = String::new();
    for (i, arg) in (1..n+1).zip(args) {
        if let Value::Integer(n) = arg.to_integer() {
            string.push(n as u8 as char);
        } else {
            return Err(Exception::UserError(Value::String(format!("bad argument #{} (number has no integer representation)", i))));
        }
    }
    Ok(vec![Value::String(string)])
}

// TODO dump, find, gmatch, gsub

fn format_(_: *mut Interpreter, _args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    unimplemented!()
}

fn len(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    // NOTE should count \0s
    match args.first() {
        Some(Value::Integer(n)) => Ok(vec![Value::Integer(n.to_string().len() as i64)]),
        Some(Value::Float(Float(x))) => Ok(vec![Value::Integer(x.to_string().len() as i64)]),
        Some(Value::String(s)) => Ok(vec![Value::Integer(s.len() as i64)]),
        _ => Err(Exception::UserError(Value::String("bad argument #1 (string expected)".to_string())))
    }
}

fn lower(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => Ok(vec![Value::String(n.to_string())]),
        Some(Value::Float(Float(x))) => Ok(vec![Value::String(x.to_string())]),
        Some(Value::String(s)) => Ok(vec![Value::String(s.to_lowercase())]),
        _ => Err(Exception::UserError(Value::String("bad argument #1 (string expected)".to_string()))),
    }
}

// TODO match, pack, packsize

fn rep(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    let string = match args.first() {
        Some(Value::Integer(n)) => n.to_string(),
        Some(Value::Float(Float(x))) => x.to_string(),
        Some(Value::String(s)) => s.clone(),
        _ => return Err(Exception::UserError(Value::String("bad argument #1 (string expected)".to_string()))),
    };
    if let Value::Integer(n) = args.get(1).unwrap_or(&Value::Nil).to_integer() {
        Ok(vec![Value::String(string.repeat(n as usize))])
    } else {
        Err(Exception::UserError(Value::String("bad argument #2 (number expected)".to_string())))
    }
}

fn reverse(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    let string = match args.first() {
        Some(Value::Integer(n)) => n.to_string(),
        Some(Value::Float(Float(x))) => x.to_string(),
        Some(Value::String(s)) => s.clone(),
        _ => return Err(Exception::UserError(Value::String("bad argument #1 (string expected)".to_string()))),
    };
    Ok(vec![Value::String(string.chars().rev().collect())])
}

fn sub(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    let string = match args.first() {
        Some(Value::Integer(n)) => n.to_string(),
        Some(Value::Float(Float(x))) => x.to_string(),
        Some(Value::String(s)) => s.clone(),
        _ => return Err(Exception::UserError(Value::String("bad argument #1 (string expected)".to_string()))),
    };
    let start = if let Value::Integer(n) = args.get(1).unwrap_or(&Value::Nil).to_integer() {
        n
    } else {
        return Err(Exception::UserError(Value::String("bad argument #2 (number expected)".to_string())));
    };
    let n = string.len() as i64;
    let end = match args.get(2) {
        None => n,
        Some(Value::Nil) => n,
        Some(val) => {
            if let Value::Integer(n) = val.to_integer() {
                n
            } else {
                return Err(Exception::UserError(Value::String("bad argument #3 (number expected)".to_string())));
            }
        }
    };
    let start = if start < -n {
        n
    } else if start < 0 {
        n + start
    } else if start == 0 {
        0
    } else if start < n {
        start - 1
    } else {
        n
    } as usize;
    let end = if end < -n {
        n
    } else if end < 0 {
        n + end + 1
    } else if end == 0 {
        0
    } else if end < n {
        end
    } else {
        n
    } as usize;
    Ok(vec![Value::String(String::from(&string[start..end]))])
}

// TODO unpack

fn upper(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    match args.first() {
        Some(Value::Integer(n)) => Ok(vec![Value::String(n.to_string())]),
        Some(Value::Float(Float(x))) => Ok(vec![Value::String(x.to_string())]),
        Some(Value::String(s)) => Ok(vec![Value::String(s.to_uppercase())]),
        _ => Err(Exception::UserError(Value::String("bad argument #1 (string expected)".to_string()))),
    }
}

fn function_object(func: fn(*mut Interpreter, Vec<Value>) -> Result<Vec<Value>, Exception>) -> Value {
    Value::Reference(ObjectReference::new(Object::Function(Function::Foreign(
        func,
    ))))
}

pub fn import_into(env: &mut Environment) {
    let mut string = Table::new();
    let _ = string.set(Value::String("byte".to_string()), function_object(byte));
    let _ = string.set(Value::String("char".to_string()), function_object(char_));
    let _ = string.set(Value::String("format".to_string()), function_object(format_));
    let _ = string.set(Value::String("len".to_string()), function_object(len));
    let _ = string.set(Value::String("lower".to_string()), function_object(lower));
    let _ = string.set(Value::String("rep".to_string()), function_object(rep));
    let _ = string.set(Value::String("reverse".to_string()), function_object(reverse));
    let _ = string.set(Value::String("sub".to_string()), function_object(sub));
    let _ = string.set(Value::String("upper".to_string()), function_object(upper));
    env.set(
        Value::String("string".to_string()),
        Value::Reference(ObjectReference::new(Object::Table(string)))
    );
}
