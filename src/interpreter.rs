use crate::basic;
use crate::env::Environment;
use crate::math;

pub struct Interpreter {
    pub env: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut env = Environment::new();
        basic::import_into(&mut env);
        math::import_into(&mut env);
        Interpreter { env }
    }

    pub fn from(env: Environment) -> Interpreter {
        Interpreter { env }
    }
}
