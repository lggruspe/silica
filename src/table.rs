use crate::ast::Exception;
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
