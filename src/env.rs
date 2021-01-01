use crate::table::Table;
use crate::value::Value;
use std::ptr;

pub struct Environment {
    pub variables: Table,
    pub parent: *mut Environment,
}

impl Default for Environment {
    fn default() -> Self {
        Environment::new()
    }
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            variables: Table::new(),
            parent: ptr::null_mut(),
        }
    }

    pub fn local(&mut self) -> Environment {
        Environment {
            variables: Table::new(),
            parent: self,
        }
    }

    pub fn get(&self, key: &Value) -> Value {
        if let Some(val) = self.variables.get(key) {
            val.clone()
        } else if self.parent.is_null() {
            Value::Nil
        } else {
            let val;
            unsafe {
                val = (*self.parent).get(key);
            }
            val
        }
    }

    pub fn set_local(&mut self, key: Value, val: Value) {
        let _ = self.variables.set(key, val);
    }

    pub fn set(&mut self, key: Value, val: Value) {
        if self.variables.get(&key).is_some() || self.parent.is_null() {
            self.set_local(key, val);
        } else {
            unsafe {
                (*self.parent).set(key, val);
            }
        }
    }

    pub fn activate<F: Fn(Environment) -> T, T>(&mut self, callback: F) -> T {
        callback(self.local())
    }
}
