use crate::function::{print_, Function};
use crate::table::Table;
use crate::value::Value;

pub struct Environment {
    pub global: Table,
    pub locals: Vec<Table>,
}

impl Environment {
    pub fn new() -> Environment {
        let mut global = Table::new();
        let _ = global.set(
            Value::String("print".to_string()),
            Value::Function(Function::Foreign(print_)),
        );
        Environment {
            global,
            locals: vec![],
        }
    }

    pub fn get(&self, name: &Value) -> &Value {
        for local in self.locals.iter().rev() {
            if let Some(val) = local.get(name) {
                return val;
            }
        }
        self.global.get(name).unwrap_or(&Value::Nil)
    }

    pub fn set(&mut self, name: Value, val: Value) {
        // find value and insert if found, otherwise insert in global
        for i in (0..self.locals.len()).rev() {
            let local = self.locals.get_mut(i);
            if let Some(local) = local {
                if local.get(&name).is_some() {
                    let _ = local.set(name, val);
                    return;
                }
            }
        }
        let _ = self.global.set(name, val);
    }

    pub fn set_local(&mut self, name: Value, val: Value) {
        if let Some(local) = self.locals.last_mut() {
            let _ = local.set(name, val);
        } else {
            let _ = self.global.set(name, val);
        }
    }
}
