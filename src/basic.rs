use crate::env::Environment;
use crate::function::Function;
use crate::interpreter::{Exception, Interpreter};
use crate::object::{Object, ObjectReference};
use crate::value::{Float, Value};

fn assert(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
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

fn error(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
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

fn next(lua: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    // TODO non-numeric indices
    let table = args.first().unwrap_or(&Value::Nil).clone();
    let index = match args.get(1) {
        Some(Value::Nil) => Value::Integer(1),
        Some(v) => v.add(&Value::Integer(1))?,
        None => Value::Integer(1),
    };
    let value;
    unsafe {
        let lua = lua.as_mut().unwrap();
        value = table.index(&index, lua)?;
    }
    if value.is_equal(&Value::Nil) {
        Ok(vec![])
    } else {
        Ok(vec![index, value])
    }
}

fn pairs(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    // TODO use __pairs metamethod if available
    Ok(vec![
        Value::Reference(ObjectReference::new(Object::Function(Function::Foreign(
            next,
        )))),
        args.first().unwrap_or(&Value::Nil).clone(),
        Value::Nil,
    ])
}

fn print_(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    if let Some(arg) = args.first() {
        print!("{}", arg.tostring());
    }
    for arg in args.iter().skip(1) {
        print!("\t{}", arg.tostring());
    }
    println!();
    Ok(vec![Value::Nil])
}

pub fn tonumber(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
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

fn tostring(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    // TODO use __tostring or __name metamethods
    if let Some(v) = args.first() {
        Ok(vec![Value::String(v.tostring())])
    } else {
        Ok(vec![Value::String(Value::Nil.tostring())])
    }
}

fn type_(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    if let Some(v) = args.first() {
        Ok(vec![Value::String(v.type_str().to_string())])
    } else {
        Ok(vec![Value::String(Value::Nil.type_str().to_string())])
    }
}

fn function_object(
    func: fn(*mut Interpreter, Vec<Value>) -> Result<Vec<Value>, Exception>,
) -> Value {
    Value::Reference(ObjectReference::new(Object::Function(Function::Foreign(
        func,
    ))))
}

fn insert(env: *mut Environment, name: &'static str, val: Value) {
    unsafe {
        (*env).set(Value::String(name.to_string()), val);
    }
}

pub fn import_into(env: *mut Environment) {
    insert(env, "assert", function_object(assert));
    insert(env, "error", function_object(error));
    insert(env, "next", function_object(next));
    insert(env, "pairs", function_object(pairs));
    insert(env, "print", function_object(print_));
    insert(env, "tonumber", function_object(tonumber));
    insert(env, "tostring", function_object(tostring));
    insert(env, "type", function_object(type_));
}

// TODO ipairs, warn
