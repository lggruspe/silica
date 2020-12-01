use crate::function::{Callable, Function};
use crate::interpreter::Interpreter;
use crate::object::{Object, ObjectReference};
use crate::table::Table;
use crate::value::{Float, Value};

#[derive(Debug)]
pub enum Exception {
    // TODO break
    RuntimeError(&'static str),
    Return(Vec<Value>),
}

pub type Argument = Vec<Expression>;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum UnaryOp {
    Minus,
    Not,
    Hash,
    Tilde,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
    Slash2,
    Caret,
    Percent,
    Ampersand,
    Tilde,
    Pipe,
    GreaterThan2,
    LessThan2,
    Dot2,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal2,
    TildeEqual,
    And,
    Or,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expression {
    Nil,
    Boolean(bool),
    String(String),
    Integer(i64),
    Float(Float),
    Dot3,
    Variable(String), // name
    Function(FunctionBody),
    Table(Vec<(Expression, Expression)>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Index(Box<Expression>, Box<Expression>), // exp[exp]
    FunctionCall(Box<Expression>, Argument),
}

pub type Block = Vec<Statement>;

pub fn exec_block(block: &Block, lua: &mut Interpreter) -> Result<(), Exception> {
    // NOTE caller should pop the stack before propagating error
    for stat in block {
        stat.exec(lua)?;
    }
    Ok(())
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionBody(pub Vec<String>, pub Block);

impl FunctionBody {
    pub fn to_method(self) -> FunctionBody {
        let mut new = self;
        new.0.insert(0, "self".to_string());
        new
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Statement {
    Assign(Vec<Expression>, Vec<Expression>), // varlist, explist
    Declare(Vec<(String, Option<String>)>, Vec<Expression>),
    Empty,
    Break,
    Goto(String),
    Do(Block),
    While(Expression, Block),
    Return(Vec<Expression>),
    GenericFor(Vec<String>, Vec<Expression>, Block),
    NumericalFor(String, Expression, Expression, Expression, Block),
    If(Expression, Block, Block),
    Label(String),
    FunctionCall(Expression, Argument),
}

impl Expression {
    pub fn eval(&self, lua: &mut Interpreter) -> Result<Value, Exception> {
        // println!("\tEVAL {:?}\n", self);
        match self {
            Expression::Nil => Ok(Value::Nil),
            Expression::Boolean(b) => Ok(Value::Boolean(*b)),
            Expression::String(s) => Ok(Value::String(s.clone())),
            Expression::Integer(n) => Ok(Value::Integer(*n)),
            Expression::Float(Float(x)) => Ok(Value::Float(Float(*x))),
            // Dot3
            Expression::Variable(name) => Ok(lua.env.get(&Value::String(name.clone())).clone()),
            // TODO closures?
            Expression::Function(body) => Ok(Value::Reference(ObjectReference::new(
                Object::Function(Function::Native(body.clone())),
            ))),
            Expression::Table(fields) => {
                let mut table = Table::new();
                for (key, val) in fields {
                    table.set(key.eval(lua)?, val.eval(lua)?)?;
                }
                Ok(Value::Reference(ObjectReference::new(Object::Table(table))))
            }
            Expression::Binary(left, binop, right) => match binop {
                BinaryOp::Plus => left.eval(lua)?.add(&right.eval(lua)?),
                BinaryOp::Minus => left.eval(lua)?.subtract(&right.eval(lua)?),
                BinaryOp::Star => left.eval(lua)?.multiply(&right.eval(lua)?),
                BinaryOp::Slash => left.eval(lua)?.float_divide(&right.eval(lua)?),
                BinaryOp::Slash2 => left.eval(lua)?.floor_divide(&right.eval(lua)?),
                BinaryOp::Caret => left.eval(lua)?.power(&right.eval(lua)?),
                BinaryOp::Percent => left.eval(lua)?.modulo(&right.eval(lua)?),
                BinaryOp::Ampersand => left.eval(lua)?.bitwise_and(&right.eval(lua)?),
                BinaryOp::Tilde => left.eval(lua)?.bitwise_xor(&right.eval(lua)?),
                BinaryOp::Pipe => left.eval(lua)?.bitwise_or(&right.eval(lua)?),
                BinaryOp::GreaterThan2 => left.eval(lua)?.bitwise_right_shift(&right.eval(lua)?),
                BinaryOp::LessThan2 => left.eval(lua)?.bitwise_left_shift(&right.eval(lua)?),
                BinaryOp::Dot2 => left.eval(lua)?.concat(&right.eval(lua)?),
                BinaryOp::LessThan => Ok(Value::Boolean(left.eval(lua)?.is_lt(&right.eval(lua)?))),
                BinaryOp::LessThanEqual => {
                    Ok(Value::Boolean(left.eval(lua)?.is_lte(&right.eval(lua)?)))
                }
                BinaryOp::GreaterThan => {
                    Ok(Value::Boolean(right.eval(lua)?.is_lt(&left.eval(lua)?)))
                }
                BinaryOp::GreaterThanEqual => {
                    Ok(Value::Boolean(right.eval(lua)?.is_lte(&left.eval(lua)?)))
                }
                BinaryOp::Equal2 => Ok(Value::Boolean(left.eval(lua)?.is_equal(&right.eval(lua)?))),
                BinaryOp::TildeEqual => {
                    Ok(Value::Boolean(!left.eval(lua)?.is_equal(&right.eval(lua)?)))
                }
                BinaryOp::And => {
                    let left = left.eval(lua)?;
                    if !left.is_truthy() {
                        Ok(left)
                    } else {
                        Ok(right.eval(lua)?)
                    }
                }
                BinaryOp::Or => {
                    let left = left.eval(lua)?;
                    if left.is_truthy() {
                        Ok(left)
                    } else {
                        Ok(right.eval(lua)?)
                    }
                }
            },
            Expression::Unary(unop, exp) => {
                let val = exp.eval(lua)?;
                match unop {
                    UnaryOp::Minus => match val {
                        Value::Integer(n) => Ok(Value::Integer(-n)),
                        Value::Float(Float(x)) => Ok(Value::Float(Float(-x))),
                        _ => Err(Exception::RuntimeError(
                            "invalid attempt to perform arithmetic",
                        )),
                    },
                    UnaryOp::Not => Ok(Value::Boolean(!val.is_truthy())),
                    UnaryOp::Hash => match val {
                        Value::String(s) => Ok(Value::Integer(s.len() as i64)), // should be no longer than max i64
                        Value::Reference(ObjectReference(o)) => match &*o.borrow() {
                            Object::Table(t) => Ok(Value::Integer(t.size())),
                            _ => unimplemented!(),
                        },
                        _ => Err(Exception::RuntimeError("invalid attempt to get length")),
                    },
                    UnaryOp::Tilde => {
                        // TODO if float can be converted to integer, do it
                        if let Value::Integer(x) = val {
                            Ok(Value::Integer(!x))
                        } else {
                            Err(Exception::RuntimeError(
                                "invalid attempt to perform bitwise operation",
                            ))
                        }
                    }
                }
            }
            Expression::Index(left, right) => {
                let (left, right) = (left.eval(lua)?, right.eval(lua)?);
                match left {
                    Value::String(_) => unimplemented!(),
                    Value::Reference(ObjectReference(o)) => match &*o.borrow() {
                        Object::Table(t) => {
                            if let Some(val) = t.get(&right) {
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
            Expression::FunctionCall(func, args) => {
                // TODO multiple return values
                // TODO callable tables
                let mut vals = Vec::new();
                for arg in args {
                    vals.push(arg.eval(lua)?);
                }
                let func = func.eval(lua)?;
                let func_ref = match func {
                    Value::Reference(ObjectReference(o)) => o,
                    _ => return Err(Exception::RuntimeError("invalid attempt to call a value")),
                };
                let func_ref = func_ref.borrow();
                let f = if let Object::Function(f) = &*func_ref {
                    f
                } else {
                    return Err(Exception::RuntimeError("invalid attempt to call a value"));
                };
                let result = f.call(lua, vals)?;
                if let Some(val) = result.first() {
                    Ok(val.clone())
                } else {
                    Ok(Value::Nil)
                }
            }
            _ => unimplemented!(),
        }
    }
}

impl Statement {
    pub fn exec(&self, lua: &mut Interpreter) -> Result<(), Exception> {
        // println!("EXEC {:?}\n", self);
        match self {
            Statement::Assign(vars, exps) => {
                let mut vals = Vec::new();
                for (_, exp) in vars.iter().zip(exps) {
                    vals.push(exp.eval(lua)?);
                }
                while vals.len() < vars.len() {
                    vals.push(Value::Nil);
                }
                for (var, val) in vars.iter().zip(vals) {
                    match var {
                        Expression::Variable(name) => {
                            lua.env.set(Value::String(name.clone()), val);
                        }
                        Expression::Index(left, right) => {
                            if let Value::Reference(ObjectReference(o)) = left.eval(lua)? {
                                // only table can be indexed in this context?
                                if let Object::Table(t) = &mut *o.borrow_mut() {
                                    let _ = t.set(right.eval(lua)?, val);
                                } else {
                                    return Err(Exception::RuntimeError(
                                        "invalid attempt to index a value",
                                    ));
                                }
                            } else {
                                return Err(Exception::RuntimeError(
                                    "invalid attempt to index a value",
                                ));
                            }
                        }
                        _ => {
                            return Err(Exception::RuntimeError("invalid attempt to index a value"))
                        }
                    }
                }
                Ok(())
            }
            Statement::Declare(atts, exps) => {
                let mut vals = Vec::new();
                for (_, exp) in atts.iter().zip(exps) {
                    vals.push(exp.eval(lua)?);
                }
                while vals.len() < atts.len() {
                    vals.push(Value::Nil);
                }
                for (attname, val) in atts.iter().zip(vals) {
                    // TODO use attributes
                    let (var, _) = attname;
                    lua.env.set_local(Value::String(var.clone()), val);
                }
                Ok(())
            }
            Statement::Empty => Ok(()),
            Statement::Break => unimplemented!(),
            // Goto(String),
            Statement::Do(block) => {
                lua.env.locals.push(Table::new());
                let result = exec_block(block, lua);
                lua.env.locals.pop();
                result?;
                Ok(())
            }
            Statement::While(exp, block) => {
                lua.env.locals.push(Table::new());
                loop {
                    match exp.eval(lua) {
                        Ok(cond) => {
                            if !cond.is_truthy() {
                                break;
                            }
                        }
                        Err(err) => {
                            lua.env.locals.pop();
                            return Err(err);
                        }
                    }
                    if let Err(err) = exec_block(block, lua) {
                        lua.env.locals.pop();
                        return Err(err);
                    }
                }
                lua.env.locals.pop();
                Ok(())
            }
            Statement::Return(exps) => {
                let mut vals = Vec::new();
                for exp in exps {
                    vals.push(exp.eval(lua)?);
                }
                Err(Exception::Return(vals))
            }
            // GenericFor(Vec<String>, Vec<Expression>, Block),
            Statement::NumericalFor(name, start, end, step, block) => {
                let start = match start.eval(lua) {
                    Ok(Value::Integer(n)) => Value::Integer(n),
                    Ok(Value::Float(Float(x))) => Value::Float(Float(x)),
                    Ok(_) => {
                        return Err(Exception::RuntimeError(
                            "bad 'for' initial value (number expected)",
                        ))
                    }
                    Err(err) => return Err(err),
                };
                let end = match end.eval(lua) {
                    Ok(Value::Integer(n)) => Value::Integer(n),
                    Ok(Value::Float(Float(x))) => Value::Float(Float(x)),
                    Ok(_) => {
                        return Err(Exception::RuntimeError("bad 'for' limit (number expected)"))
                    }
                    Err(err) => return Err(err),
                };
                let step = step.eval(lua)?;
                let binop = match step {
                    Value::Integer(0) => return Err(Exception::RuntimeError("'for' step is zero")),
                    Value::Integer(n) => {
                        if n < 0 {
                            BinaryOp::GreaterThanEqual
                        } else {
                            BinaryOp::LessThanEqual
                        }
                    }
                    Value::Float(Float(x)) => {
                        if x == 0.0 {
                            return Err(Exception::RuntimeError("'for' step is zero"));
                        } else if x < 0.0 {
                            BinaryOp::GreaterThanEqual
                        } else {
                            BinaryOp::LessThanEqual
                        }
                    }
                    _ => return Err(Exception::RuntimeError("bad 'for' step (number expected)")),
                };
                let (start, step, end) = match (start, step) {
                    (Value::Integer(start), Value::Integer(step)) => {
                        (Value::Integer(start), Value::Integer(step), end)
                    }
                    (start, step) => (start.to_float(), step.to_float(), end.to_float()),
                };
                lua.env.locals.push(Table::new());
                lua.env
                    .set_local(Value::String(name.clone()), start.clone());
                loop {
                    let i = Expression::Variable(name.clone()).eval(lua).unwrap();
                    match binop {
                        BinaryOp::LessThanEqual => {
                            if !i.is_lte(&end) {
                                break;
                            }
                        }
                        BinaryOp::GreaterThanEqual => {
                            if i.is_lt(&end) {
                                break;
                            }
                        }
                        _ => panic!(),
                    }
                    if let Err(err) = exec_block(block, lua) {
                        lua.env.locals.pop();
                        return Err(err);
                    }
                    let i = match i.add(&step) {
                        Ok(i) => i,
                        Err(e) => {
                            lua.env.locals.pop();
                            return Err(e);
                        }
                    };
                    lua.env.set_local(Value::String(name.clone()), i);
                }
                lua.env.locals.pop();
                Ok(())
            }
            Statement::If(exp, then_block, else_block) => {
                lua.env.locals.push(Table::new());
                // TODO pop stack before propagating Err from exp.eval?
                let result = if exp.eval(lua)?.is_truthy() {
                    exec_block(then_block, lua)
                } else {
                    exec_block(else_block, lua)
                };
                lua.env.locals.pop();
                result?;
                Ok(())
            }
            // Label(String),
            Statement::FunctionCall(func, args) => {
                if let Err(Exception::RuntimeError(msg)) =
                    Expression::FunctionCall(Box::new(func.clone()), args.clone()).eval(lua)
                {
                    return Err(Exception::RuntimeError(msg));
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}
