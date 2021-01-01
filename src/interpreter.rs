use crate::basic;
use crate::env::Environment;
use crate::math;
use crate::string;
use crate::table;

pub struct Interpreter {
    pub env: Environment,
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter::new()
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut env = Environment::new();
        basic::import_into(&mut env);
        math::import_into(&mut env);
        string::import_into(&mut env);
        table::import_into(&mut env);
        Interpreter { env }
    }

    pub fn from(env: Environment) -> Interpreter {
        Interpreter { env }
    }
}
