use crate::ast::Exception;
use crate::env::Environment;
use crate::function::Function;
use crate::interpreter::Interpreter;
use crate::object::{Object, ObjectReference};
use crate::value::{Float, Value};
use std::collections::{BTreeSet, HashMap};
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Table {
    indices: BTreeSet<i64>,
    pairs: HashMap<Value, Value>,
}

impl Table {
    pub fn new() -> Table {
        Table {
            indices: BTreeSet::new(),
            pairs: HashMap::new(),
        }
    }

    pub fn get(&self, key: &Value) -> Option<&Value> {
        self.pairs.get(key)
    }
    // TODO get_var(&self, key: &str) and set_var(&mut self, key: &str, val: Value)

    pub fn set(&mut self, key: Value, val: Value) -> Result<Option<Value>, Exception> {
        // nil and nan key not allowed
        // a nil Value removes the key from the table
        if val == Value::Nil {
            if let Value::Integer(n) = key {
                if n > 0 {
                    self.indices.remove(&n);
                }
            }
            return Ok(self.pairs.remove(&key));
        }
        match key {
            Value::Nil => Err(Exception::RuntimeError("table index is nil")),
            Value::Float(Float(x)) => {
                if x.is_nan() {
                    Err(Exception::RuntimeError("table index is NaN"))
                } else {
                    Ok(self.pairs.insert(key, val))
                }
            }
            Value::Integer(n) => {
                if n > 0 {
                    self.indices.insert(n);
                }
                Ok(self.pairs.insert(key, val))
            }
            _ => Ok(self.pairs.insert(key, val)),
        }
    }

    pub fn size(&self) -> i64 {
        // should be O(log n), n is the largest nonnegative
        // always returns the largest border
        match self.indices.iter().next_back() {
            Some(&n) => n,
            None => 0,
        }
    }
}

impl Hash for Table {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        unimplemented!(); // TODO
    }
}

/*
fn concat(lua: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    let list = if let Some(list) = args.first() {
        list
    } else {
        return Err(Exception::UserError(Value::String("bad argument #1 (table expected)".to_string())));
    };
    let sep = match args.get(1) {
        None => Value::String("".to_string()),
        Some(Value::Nil) => Value::String("".to_string()),
        Some(Value::Integer(n)) => Value::String(n.to_string()),
        Some(Value::Float(Float(x))) => Value::String(x.to_string()),
        Some(Value::String(s)) => Value::String(s.clone()),
        _ => return Err(Exception::UserError(Value::String("bad argument #2 (string expected)".to_string()))),
    };
    let start = match args.get(2) {
        None => 1,
        Some(Value::Nil) => 1,
        Some(val) => {
            if let Value::Integer(n) = val.to_integer() {
                n
            } else {
                return Err(Exception::UserError(Value::String(
                        "bad argument #3 (number expected)".to_string(),
                    )));
            }
        }
    };
    let n = match list {
        Value::Reference(ObjectReference(o)) => match &*o.borrow() {
            Object::Table(t) => t.size(),
            _ => return Err(Exception::UserError(Value::String(
                "bad argument #1 (table expected)".to_string(),
            ))),
        }
        _ => return Err(Exception::UserError(Value::String(
                "bad argument #1 (table expected)".to_string(),
            ))),
    };
    let end = match args.get(3) {
        None => n,
        Some(Value::Nil) => n,
        Some(val) => {
            if let Value::Integer(n) = val.to_integer() {
                n
            } else {
                return Err(Exception::UserError(Value::String(
                        "bad argument #4 (number expected)".to_string(),
                    )));
            }
        }
    };
    // TODO check indices
    // TODO FIXME
    let mut string = Value::String(String::new());
    let start = start - 1;
    for i in start..end {
        unsafe {
            let mut lua = lua.as_mut().unwrap();
            match list.index(&Value::Integer(i), &mut lua) {
                Ok(val) => {
                    match string.concat(&sep).unwrap().concat(&val) {
                        Ok(new) => string = new,
                        Err(_) => return Err(Exception::UserError(Value::String(format!("invalid value at index {} in table", i + 1)))),
                    }
                }
                Err(err) => return Err(err),
            }
        }
    }
    Ok(vec![string])
}
*/

fn insert(_: *mut Interpreter, _: Vec<Value>) -> Result<Vec<Value>, Exception> {
    unimplemented!()
}

fn move_(_: *mut Interpreter, _: Vec<Value>) -> Result<Vec<Value>, Exception> {
    unimplemented!()
}

fn pack(_: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    let n = args.len() as i64;
    let mut table = Table::new();
    let _ = table.set(Value::String("n".to_string()), Value::Integer(n));
    for (i, arg) in (1..n + 1).zip(args) {
        let _ = table.set(Value::Integer(i), arg);
    }
    Ok(vec![Value::Reference(ObjectReference::new(Object::Table(
        table,
    )))])
}

fn remove(_: *mut Interpreter, _: Vec<Value>) -> Result<Vec<Value>, Exception> {
    unimplemented!()
}

fn sort(_: *mut Interpreter, _args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    // TODO use comparison function (args[1]); comp order must be assymetric and transitive
    /*
    let list = args.first().unwrap_or(&Value::Nil);
    if list.type_str() != "table" {
        return Err(Exception::UserError(Value::String("bad argument #1 (table expected)".to_string())));
    }
    */
    Ok(vec![Value::Nil])
}

fn unpack(lua: *mut Interpreter, args: Vec<Value>) -> Result<Vec<Value>, Exception> {
    let table = args.first().unwrap_or(&Value::Nil);
    let start = if let Value::Integer(n) = args.get(1).unwrap_or(&Value::Nil).to_integer() {
        n
    } else {
        return Err(Exception::UserError(Value::String(
            "bad argument #1 (number expected)".to_string(),
        )));
    };
    let end = if let Value::Integer(n) = args.get(2).unwrap_or(&Value::Nil).to_integer() {
        n
    } else {
        return Err(Exception::UserError(Value::String(
            "bad argument #2 (number expected)".to_string(),
        )));
    };
    // TODO check index
    let mut result = Vec::new();
    unsafe {
        for i in start - 1..end {
            let mut lua = lua.as_mut().unwrap();
            let index = Value::Integer(i);
            result.push(table.index(&index, &mut lua).unwrap_or(Value::Nil));
        }
    }
    Ok(result)
}

fn function_object(
    func: fn(*mut Interpreter, Vec<Value>) -> Result<Vec<Value>, Exception>,
) -> Value {
    Value::Reference(ObjectReference::new(Object::Function(Function::Foreign(
        func,
    ))))
}

pub fn import_into(env: &mut Environment) {
    let mut table = Table::new();
    // let _ = table.set(Value::String("concat".to_string()), function_object(concat));
    let _ = table.set(Value::String("insert".to_string()), function_object(insert));
    let _ = table.set(Value::String("move".to_string()), function_object(move_));
    let _ = table.set(Value::String("pack".to_string()), function_object(pack));
    let _ = table.set(Value::String("remove".to_string()), function_object(remove));
    let _ = table.set(Value::String("sort".to_string()), function_object(sort));
    let _ = table.set(Value::String("unpack".to_string()), function_object(unpack));
    env.set(
        Value::String("table".to_string()),
        Value::Reference(ObjectReference::new(Object::Table(table))),
    );
}
