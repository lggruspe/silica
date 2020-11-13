use crate::env::Environment;

pub struct Interpreter {
    pub env: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Environment::new(),
        }
    }
}
