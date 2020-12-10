use crate::ast::{Exception, LuaResult};
use crate::object::{Object, ObjectReference};
use std::hash::{Hash, Hasher};

/// Integer overflow should cause the value to wrap around 2C-style.
/// Strings are immutable.
/// - Can contain any 8-bit value (like \0)
/// - Encoding agnostic
/// - Length of any string must fit an integer

/// Functions can be implemented in Lua or C.

/// Userdata: block of raw memory
/// - Can be full (managed by Lua)
/// - or light
/// They cannot be created or modified in Lua, only through the C API.

#[derive(Clone, Debug)]
pub struct Float(pub f64);

impl Hash for Float {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_ne_bytes().hash(state)
    }
}

impl PartialEq for Float {
    fn eq(&self, other: &Float) -> bool {
        self.0.to_ne_bytes() == other.0.to_ne_bytes()
    }
}

impl Eq for Float {}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Value {
    Nil,
    Boolean(bool),
    Integer(i64), // ints and floats are both numbers
    Float(Float),
    String(String),
    Reference(ObjectReference),
}

impl Value {
    pub fn into_luaresult(self) -> LuaResult {
        LuaResult::One(self)
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(false) => false,
            _ => true,
        }
    }

    pub fn type_str(&self) -> &str {
        match self {
            Value::Nil => "nil",
            Value::Boolean(_) => "boolean",
            Value::Float(_) => "number",
            Value::Integer(_) => "number",
            Value::String(_) => "string",
            Value::Reference(o) => o.type_str(),
        }
    }

    pub fn tostring(&self) -> String {
        match self {
            Value::Nil => "nil".to_string(),
            Value::Boolean(true) => "true".to_string(),
            Value::Boolean(false) => "false".to_string(),
            Value::Integer(n) => n.to_string(),
            Value::Float(Float(x)) => x.to_string(),
            Value::String(s) => s.clone(),
            Value::Reference(o) => o.tostring(),
        }
    }

    pub fn is_raw_equal(&self, _other: &Value) -> bool {
        // compare without metamethods
        unimplemented!()
    }

    pub fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            // TODO float and integer
            (Value::String(a), Value::String(b)) => a == b,
            // TODO Objects should be compared by reference
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Integer(_), Value::Float(_)) => unimplemented!(),
            (Value::Float(_), Value::Integer(_)) => unimplemented!(),
            (Value::Float(_), Value::Float(_)) => unimplemented!(),
            _ => false,
        }
    }

    pub fn concat(&self, other: &Value) -> Result<Value, Exception> {
        let left = match self {
            Value::Float(Float(x)) => x.to_string(),
            Value::Integer(n) => n.to_string(),
            Value::String(s) => s.clone(),
            _ => return Err(Exception::RuntimeError("invalid attempt to concatenate")),
        };
        match other {
            Value::Float(Float(x)) => Ok(Value::String(format!("{}{}", left, x))),
            Value::Integer(n) => Ok(Value::String(format!("{}{}", left, n))),
            Value::String(s) => Ok(Value::String(format!("{}{}", left, s))),
            _ => Err(Exception::RuntimeError("invalid attempt to concatenate")),
        }
        // TODO __concat metamethod
    }

    pub fn is_lt(&self, other: &Value) -> bool {
        // TODO __lt metamethod
        // TODO string comparison
        // TODO int float comparison
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a < b,
            (Value::Float(Float(a)), Value::Float(Float(b))) => a < b,
            _ => unimplemented!(),
        }
    }

    pub fn is_lte(&self, other: &Value) -> bool {
        // TODO __le metamethod
        // TODO string comparison
        // TODO int float comparison
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a <= b,
            (Value::Float(Float(a)), Value::Float(Float(b))) => a <= b,
            _ => unimplemented!(),
        }
    }

    pub fn bitwise_and(&self, other: &Value) -> Result<Value, Exception> {
        // TODO convert float to int if there's an int representation
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a & b)),
            _ => Err(Exception::RuntimeError(
                "invalid attempt to perform bitwise operation",
            )),
        }
    }

    pub fn bitwise_or(&self, other: &Value) -> Result<Value, Exception> {
        // TODO convert float to int if there's an int representation
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a | b)),
            _ => Err(Exception::RuntimeError(
                "invalid attempt to perform bitwise operation",
            )),
        }
    }

    pub fn bitwise_xor(&self, other: &Value) -> Result<Value, Exception> {
        // TODO convert float to int if there's an int representation
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a ^ b)),
            _ => Err(Exception::RuntimeError(
                "invalid attempt to perform bitwise operation",
            )),
        }
    }

    pub fn bitwise_right_shift(&self, other: &Value) -> Result<Value, Exception> {
        // TODO convert float to int if there's an int representation
        // TODO see reference manual
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a >> b)),
            _ => Err(Exception::RuntimeError(
                "invalid attempt to perform bitwise operation",
            )),
        }
    }

    pub fn bitwise_left_shift(&self, other: &Value) -> Result<Value, Exception> {
        // TODO convert float to int if there's an int representation
        // TODO see reference manual
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a << b)),
            _ => Err(Exception::RuntimeError(
                "invalid attempt to perform bitwise operation",
            )),
        }
    }

    pub fn index(&self, other: &Value) -> Result<Value, Exception> {
        match self {
            Value::String(_) => unimplemented!("index string value"),
            Value::Reference(ObjectReference(o)) => match &*o.borrow() {
                Object::Table(t) => {
                    if let Some(val) = t.get(other) {
                        Ok(val.clone())
                    } else {
                        Ok(Value::Nil)
                    }
                }
                Object::UserData => unimplemented!(),
                // TODO Thread?
                _ => Err(Exception::RuntimeError("invalid attempt to index a value")),
            },
            _ => Err(Exception::RuntimeError("invalid attempt to index a value")), // can't index nil, bool, number, function, thread
        }
    }

    pub fn add(&self, other: &Value) -> Result<Value, Exception> {
        // TODO handle integer overflow (see reference manual)
        use Value::Integer;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a + b)),
            (Integer(a), Value::Float(Float(b))) => Ok(Value::Float(Float(*a as f64 + b))),
            (Value::Float(Float(a)), Integer(b)) => Ok(Value::Float(Float(a + *b as f64))),
            _ => Err(Exception::RuntimeError("invalid attempt to add values")),
        }
    }

    pub fn subtract(&self, other: &Value) -> Result<Value, Exception> {
        // TODO handle integer overflow (see reference manual)
        use Value::Integer;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a - b)),
            (Integer(a), Value::Float(Float(b))) => Ok(Value::Float(Float(*a as f64 - b))),
            (Value::Float(Float(a)), Integer(b)) => Ok(Value::Float(Float(a - *b as f64))),
            _ => Err(Exception::RuntimeError("invalid attempt to sub values")),
        }
    }

    pub fn multiply(&self, other: &Value) -> Result<Value, Exception> {
        // TODO handle integer overflow (see reference manual)
        use Value::Integer;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a * b)),
            (Integer(a), Value::Float(Float(b))) => Ok(Value::Float(Float(*a as f64 * b))),
            (Value::Float(Float(a)), Integer(b)) => Ok(Value::Float(Float(a * *b as f64))),
            _ => Err(Exception::RuntimeError(
                "invalid attempt to perform arithmetic",
            )),
        }
    }

    pub fn floor_divide(&self, other: &Value) -> Result<Value, Exception> {
        use Value::Integer;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a / b)),
            (Integer(a), Value::Float(Float(b))) => {
                Ok(Value::Float(Float((*a as f64 / *b).floor())))
            }
            (Value::Float(Float(a)), Integer(b)) => {
                Ok(Value::Float(Float((*a / *b as f64).floor())))
            }
            _ => Err(Exception::RuntimeError(
                "invalid attempt to perform arithmetic",
            )),
        }
    }

    pub fn modulo(&self, other: &Value) -> Result<Value, Exception> {
        // TODO Compare lua modulo and rust modulo
        // TODO see reference manual
        use Value::Integer;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a % b)),
            (Integer(a), Value::Float(Float(b))) => Ok(Value::Float(Float(*a as f64 % b))),
            (Value::Float(Float(a)), Integer(b)) => Ok(Value::Float(Float(a % *b as f64))),
            _ => Err(Exception::RuntimeError(
                "invalid attempt to perform arithmetic",
            )),
        }
    }

    pub fn power(&self, other: &Value) -> Result<Value, Exception> {
        use Value::Integer;
        let a = match self {
            Integer(a) => *a as f64,
            Value::Float(Float(a)) => *a,
            _ => {
                return Err(Exception::RuntimeError(
                    "invalid attempt to perform arithmetic",
                ))
            }
        };
        let b = match other {
            Integer(a) => *a as f64,
            Value::Float(Float(a)) => *a,
            _ => {
                return Err(Exception::RuntimeError(
                    "invalid attempt to perform arithmetic",
                ))
            }
        };
        Ok(Value::Float(Float(a.powf(b))))
    }

    pub fn float_divide(&self, other: &Value) -> Result<Value, Exception> {
        use Value::Integer;
        let a = match self {
            Integer(a) => *a as f64,
            Value::Float(Float(a)) => *a,
            _ => {
                return Err(Exception::RuntimeError(
                    "invalid attempt to perform arithmetic",
                ))
            }
        };
        let b = match other {
            Integer(a) => *a as f64,
            Value::Float(Float(a)) => *a,
            _ => {
                return Err(Exception::RuntimeError(
                    "invalid attempt to perform arithmetic",
                ))
            }
        };
        Ok(Value::Float(Float(a / b)))
    }

    pub fn to_float(&self) -> Value {
        // panics if self is not a number
        match self {
            Value::Integer(n) => Value::Float(Float(*n as f64)),
            Value::Float(_) => self.clone(),
            _ => panic!(),
        }
    }
}
