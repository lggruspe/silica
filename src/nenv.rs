use crate::table::Table;
use crate::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

pub type Environment = Rc<RefCell<RawEnvironment>>;

pub struct RawEnvironment {
    pub variables: Table,
    pub parent: Option<Environment>,
}

trait EnvironmentTrait {
    /// Creates an empty environment.
    fn new() -> Self;

    /// Creates a local environment on top of the current one.
    fn local(&self) -> Self;

    /// Gets value assigned to key from local or parent environments.
    fn get(&self, key: &Value) -> Value;

    /// Assigns `val` to `key` locally.
    fn set_local(&self, key: Value, val: Value);

    /// Assigns `val` to `key`.
    fn set(&self, key: Value, val: Value);
}

impl EnvironmentTrait for Environment {
    fn new() -> Self {
        Rc::new(RefCell::new(RawEnvironment {
            variables: Table::new(),
            parent: None,
        }))
    }

    fn local(&self) -> Self {
        Rc::new(RefCell::new(RawEnvironment {
            variables: Table::new(),
            parent: Some(Rc::clone(self)),
        }))
    }

    fn get(&self, key: &Value) -> Value {
        if let Some(val) = self.borrow().variables.get(key) {
            val.clone()
        } else if let Some(env) = &self.borrow().parent {
            env.get(key).clone()
        } else {
            Value::Nil
        }
    }

    fn set_local(&self, key: Value, val: Value) {
        let _ = self.borrow_mut().variables.set(key, val);
    }

    fn set(&self, key: Value, val: Value) {
        if self.borrow().variables.get(&key).is_some() {
            let _ = self.borrow_mut().variables.set(key, val);
        } else if let Some(parent) = &self.borrow().parent {
            parent.set(key, val);
        } else {
            let _ = self.borrow_mut().variables.set(key, val);
        }
    }
}
