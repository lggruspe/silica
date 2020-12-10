use crate::function::{Callable, Function};
use crate::interpreter::Interpreter;
use crate::object::{Object, ObjectReference};
use crate::table::Table;
use crate::value::{Float, Value};
use std::cmp::{max, min};

pub enum LuaResult {
    One(Value),
    Many(Vec<Value>),
}

impl LuaResult {
    pub fn first(&self) -> Value {
        match self {
            LuaResult::One(val) => val.clone(),
            LuaResult::Many(vals) => vals.first().unwrap_or(&Value::Nil).clone(),
        }
    }
}

#[derive(Debug)]
pub enum Exception {
    // TODO break
    RuntimeError(&'static str),
    Return(Vec<Value>),
    UserError(Value),
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
    pub fn eval(&self, lua: &mut Interpreter) -> Result<LuaResult, Exception> {
        // println!("\tEVAL {:?}\n", self);
        match self {
            Expression::Nil => Ok(LuaResult::One(Value::Nil)),
            Expression::Boolean(b) => Ok(LuaResult::One(Value::Boolean(*b))),
            Expression::String(s) => Ok(LuaResult::One(Value::String(s.clone()))),
            Expression::Integer(n) => Ok(LuaResult::One(Value::Integer(*n))),
            Expression::Float(Float(x)) => Ok(LuaResult::One(Value::Float(Float(*x)))),
            // Dot3
            Expression::Variable(name) => Ok(LuaResult::One(
                lua.env.get(&Value::String(name.clone())).clone(),
            )),
            // TODO closures?
            Expression::Function(body) => Ok(LuaResult::One(Value::Reference(
                ObjectReference::new(Object::Function(Function::Native(body.clone()))),
            ))),
            Expression::Table(fields) => {
                let mut table = Table::new();
                for (key, val) in fields {
                    let key = match key.eval(lua) {
                        Ok(key) => key.first(),
                        Err(err) => return Err(err),
                    };
                    let val = match val.eval(lua) {
                        Ok(val) => val.first(),
                        Err(err) => return Err(err),
                    };
                    if let Err(err) = table.set(key, val) {
                        return Err(err);
                    }
                }
                Ok(LuaResult::One(Value::Reference(ObjectReference::new(
                    Object::Table(table),
                ))))
            }
            Expression::Binary(left, binop, right) => match binop {
                BinaryOp::Plus => Ok(left
                    .eval(lua)?
                    .first()
                    .add(&right.eval(lua)?.first())?
                    .into_luaresult()),
                // BinaryOp::Plus => left.eval(lua)?.add(&right.eval(lua)?),
                BinaryOp::Minus => Ok(left
                    .eval(lua)?
                    .first()
                    .subtract(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::Star => Ok(left
                    .eval(lua)?
                    .first()
                    .multiply(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::Slash => Ok(left
                    .eval(lua)?
                    .first()
                    .float_divide(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::Slash2 => Ok(left
                    .eval(lua)?
                    .first()
                    .floor_divide(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::Caret => Ok(left
                    .eval(lua)?
                    .first()
                    .power(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::Percent => Ok(left
                    .eval(lua)?
                    .first()
                    .modulo(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::Ampersand => Ok(left
                    .eval(lua)?
                    .first()
                    .bitwise_and(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::Tilde => Ok(left
                    .eval(lua)?
                    .first()
                    .bitwise_xor(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::Pipe => Ok(left
                    .eval(lua)?
                    .first()
                    .bitwise_or(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::GreaterThan2 => Ok(left
                    .eval(lua)?
                    .first()
                    .bitwise_right_shift(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::LessThan2 => Ok(left
                    .eval(lua)?
                    .first()
                    .bitwise_left_shift(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::Dot2 => Ok(left
                    .eval(lua)?
                    .first()
                    .concat(&right.eval(lua)?.first())?
                    .into_luaresult()),
                BinaryOp::LessThan => Ok(LuaResult::One(Value::Boolean(
                    left.eval(lua)?.first().is_lt(&right.eval(lua)?.first()),
                ))),
                BinaryOp::LessThanEqual => Ok(LuaResult::One(Value::Boolean(
                    left.eval(lua)?.first().is_lte(&right.eval(lua)?.first()),
                ))),
                BinaryOp::GreaterThan => Ok(LuaResult::One(Value::Boolean(
                    right.eval(lua)?.first().is_lt(&left.eval(lua)?.first()),
                ))),
                BinaryOp::GreaterThanEqual => Ok(LuaResult::One(Value::Boolean(
                    right.eval(lua)?.first().is_lte(&left.eval(lua)?.first()),
                ))),
                BinaryOp::Equal2 => Ok(LuaResult::One(Value::Boolean(
                    left.eval(lua)?.first().is_equal(&right.eval(lua)?.first()),
                ))),
                BinaryOp::TildeEqual => Ok(LuaResult::One(Value::Boolean(
                    !left.eval(lua)?.first().is_equal(&right.eval(lua)?.first()),
                ))),
                BinaryOp::And => {
                    let left = match left.eval(lua) {
                        Ok(val) => val.first(),
                        Err(err) => return Err(err),
                    };
                    if !left.is_truthy() {
                        Ok(LuaResult::One(left))
                    } else {
                        Ok(right.eval(lua)?)
                    }
                }
                BinaryOp::Or => {
                    let left = match left.eval(lua) {
                        Ok(val) => val.first(),
                        Err(err) => return Err(err),
                    };
                    if left.is_truthy() {
                        Ok(LuaResult::One(left))
                    } else {
                        Ok(right.eval(lua)?)
                    }
                }
            },
            Expression::Unary(unop, exp) => {
                let val = match exp.eval(lua) {
                    Ok(val) => val.first(),
                    Err(err) => return Err(err),
                };
                match unop {
                    UnaryOp::Minus => match val {
                        Value::Integer(n) => Ok(LuaResult::One(Value::Integer(-n))),
                        Value::Float(Float(x)) => Ok(LuaResult::One(Value::Float(Float(-x)))),
                        _ => Err(Exception::RuntimeError(
                            "invalid attempt to perform arithmetic",
                        )),
                    },
                    UnaryOp::Not => Ok(LuaResult::One(Value::Boolean(!val.is_truthy()))),
                    UnaryOp::Hash => match val {
                        Value::String(s) => Ok(LuaResult::One(Value::Integer(s.len() as i64))), // should be no longer than max i64
                        Value::Reference(ObjectReference(o)) => match &*o.borrow() {
                            Object::Table(t) => Ok(LuaResult::One(Value::Integer(t.size()))),
                            _ => unimplemented!(),
                        },
                        _ => Err(Exception::RuntimeError("invalid attempt to get length")),
                    },
                    UnaryOp::Tilde => {
                        // TODO if float can be converted to integer, do it
                        if let Value::Integer(x) = val {
                            Ok(LuaResult::One(Value::Integer(!x)))
                        } else {
                            Err(Exception::RuntimeError(
                                "invalid attempt to perform bitwise operation",
                            ))
                        }
                    }
                }
            }
            Expression::Index(left, right) => {
                let left = match left.eval(lua) {
                    Ok(val) => val.first(),
                    Err(err) => return Err(err),
                };
                let right = match right.eval(lua) {
                    Ok(val) => val.first(),
                    Err(err) => return Err(err),
                };
                Ok(left.index(&right)?.into_luaresult())
            }
            Expression::FunctionCall(func, args) => {
                // TODO callable tables
                let count = max(1, args.len());
                let mut i = 1;
                let mut vals = Vec::new();
                for arg in args {
                    if i != count {
                        vals.push(match arg.eval(lua) {
                            Ok(arg) => arg.first(),
                            Err(err) => return Err(err),
                        });
                    } else {
                        match arg.eval(lua) {
                            Ok(LuaResult::One(val)) => vals.push(val),
                            Ok(LuaResult::Many(val)) => vals.append(&mut val.clone()),
                            Err(err) => return Err(err),
                        }
                    }
                    i += 1;
                }
                let func = match func.eval(lua) {
                    Ok(val) => val.first(),
                    Err(err) => return Err(err),
                };
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
                match f.call(lua, vals) {
                    Ok(res) => match res.len() {
                        0 => Ok(LuaResult::One(Value::Nil)),
                        1 => Ok(LuaResult::One(res.first().unwrap().clone())),
                        _ => Ok(LuaResult::Many(res)),
                    },
                    Err(err) => Err(err),
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
                let count = max(1, min(vars.len(), exps.len()));
                let mut i = 1;
                let mut vals = Vec::new();
                for (_, exp) in vars.iter().zip(exps) {
                    if i != count {
                        vals.push(match exp.eval(lua) {
                            Ok(val) => val.first(),
                            Err(err) => return Err(err),
                        });
                    } else {
                        match exp.eval(lua) {
                            Ok(LuaResult::One(val)) => vals.push(val),
                            Ok(LuaResult::Many(val)) => vals.append(&mut val.clone()),
                            Err(err) => return Err(err),
                        }
                    }
                    i += 1;
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
                            let left = match left.eval(lua) {
                                Ok(val) => val.first(),
                                Err(err) => return Err(err),
                            };
                            let right = match right.eval(lua) {
                                Ok(val) => val.first(),
                                Err(err) => return Err(err),
                            };
                            if let Value::Reference(ObjectReference(o)) = left {
                                // only table can be indexed in this context?
                                if let Object::Table(t) = &mut *o.borrow_mut() {
                                    let _ = t.set(right, val);
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
                let count = max(1, min(atts.len(), exps.len()));
                let mut i = 1;
                let mut vals = Vec::new();
                for (_, exp) in atts.iter().zip(exps) {
                    if i != count {
                        vals.push(match exp.eval(lua) {
                            Ok(val) => val.first(),
                            Err(err) => return Err(err),
                        });
                    } else {
                        match exp.eval(lua) {
                            Ok(LuaResult::One(val)) => vals.push(val),
                            Ok(LuaResult::Many(val)) => vals.append(&mut val.clone()),
                            Err(err) => return Err(err),
                        }
                    }
                    i += 1;
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
                            if !cond.first().is_truthy() {
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
                    vals.push(match exp.eval(lua) {
                        Ok(val) => val.first(),
                        Err(err) => return Err(err),
                    });
                }
                Err(Exception::Return(vals))
            }
            Statement::GenericFor(namelist, explist, block) => {
                // TODO use closing value (explist[4])
                let control = Value::String(namelist.first().unwrap().clone());
                let state = if let Some(exp) = explist.get(1) {
                    exp.clone()
                } else {
                    Expression::Nil
                };
                let init_val = if let Some(exp) = explist.get(2) {
                    match exp.eval(lua) {
                        Ok(val) => val.first(),
                        Err(err) => return Err(err),
                    }
                } else {
                    Value::Nil
                };
                lua.env.locals.push(Table::new());
                lua.env.set_local(control, init_val);
                loop {
                    let vals = Expression::FunctionCall(
                        Box::new(explist.first().unwrap().clone()),
                        vec![
                            state.clone(),
                            Expression::Variable(namelist.first().unwrap().clone()),
                        ],
                    )
                    .eval(lua);
                    match vals {
                        Ok(LuaResult::One(val)) => {
                            let control = Value::String(namelist.first().unwrap().clone());
                            lua.env.set_local(control, val);
                        }
                        Ok(LuaResult::Many(res)) => {
                            let mut i = 0;
                            for (name, val) in namelist.iter().zip(res) {
                                i += 1;
                                lua.env.set_local(Value::String(name.clone()), val);
                            }
                            while i < namelist.len() {
                                let name = namelist.get(i).unwrap().clone();
                                lua.env.set_local(Value::String(name), Value::Nil);
                                i += 1;
                            }
                        }
                        Err(err) => {
                            lua.env.locals.pop();
                            return Err(err);
                        }
                    }
                    let control = Value::String(namelist.first().unwrap().clone());
                    if let Value::Nil = *lua.env.get(&control) {
                        lua.env.locals.pop();
                        return Ok(());
                    }
                    if let Err(err) = exec_block(block, lua) {
                        lua.env.locals.pop();
                        return Err(err);
                    }
                }
            }
            Statement::NumericalFor(name, start, end, step, block) => {
                let start = match start.eval(lua) {
                    Ok(val) => val.first(),
                    Err(err) => return Err(err),
                };
                let start = match start {
                    Value::Integer(n) => Value::Integer(n),
                    Value::Float(Float(x)) => Value::Float(Float(x)),
                    _ => {
                        return Err(Exception::RuntimeError(
                            "bad 'for' initial value (number expected)",
                        ))
                    }
                };
                let end = match end.eval(lua) {
                    Ok(val) => val.first(),
                    Err(err) => return Err(err),
                };
                let end = match end {
                    Value::Integer(n) => Value::Integer(n),
                    Value::Float(Float(x)) => Value::Float(Float(x)),
                    _ => return Err(Exception::RuntimeError("bad 'for' limit (number expected)")),
                };
                let step = match step.eval(lua) {
                    Ok(step) => step.first(),
                    Err(err) => return Err(err),
                };
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
                    let i = Expression::Variable(name.clone())
                        .eval(lua)
                        .unwrap()
                        .first();
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
                let result = if exp.eval(lua)?.first().is_truthy() {
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
                match Expression::FunctionCall(Box::new(func.clone()), args.clone()).eval(lua) {
                    // TODO propagate Exception::Break
                    Err(Exception::RuntimeError(msg)) => Err(Exception::RuntimeError(msg)),
                    Err(Exception::UserError(msg)) => Err(Exception::UserError(msg)),
                    _ => Ok(()),
                }
            }
            _ => Ok(()),
        }
    }
}
