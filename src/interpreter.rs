use crate::ast::{BinaryOp, Expression, Statement, UnaryOp};
use crate::basic;
use crate::env::Environment;
use crate::function::Function;
use crate::math;
use crate::object::{Object, ObjectReference};
use crate::string;
use crate::table;
use crate::table::Table;
use crate::value::{Float, Value};

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

#[derive(Debug)]
pub enum Exception {
    // TODO break
    RuntimeError(&'static str),
    Return(Vec<Value>),
    UserError(Value),
}

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
            Expression::Variable(name) => {
                Ok(LuaResult::One(lua.env.get(&Value::String(name.clone()))))
            }
            // TODO closures?
            Expression::Function(body) => Ok(LuaResult::One(Value::Reference(
                ObjectReference::new(Object::Function(Function::Native {
                    body: body.clone(),
                    parent: &mut lua.env,
                })),
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
                Ok(left.index(&right, lua)?.into_luaresult())
            }
            Expression::FunctionCall(func, args) => {
                // TODO callable tables
                let vals = match eval_list(args, lua) {
                    Ok(LuaResult::One(val)) => vec![val],
                    Ok(LuaResult::Many(vals)) => vals,
                    Err(err) => return Err(err),
                };
                let func = match func.eval(lua) {
                    Ok(val) => val.first(),
                    Err(err) => return Err(err),
                };
                func.call(lua, vals)
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
                let count = std::cmp::max(1, std::cmp::min(vars.len(), exps.len()));
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
                let count = std::cmp::max(1, std::cmp::min(atts.len(), exps.len()));
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
            Statement::Do(block) => lua
                .env
                .activate(|env| exec_block(block, &mut Interpreter::from(env))),
            Statement::While(exp, block) => lua.env.activate(|env| {
                let mut lua = Interpreter::from(env);
                loop {
                    match exp.eval(&mut lua) {
                        Ok(cond) => {
                            if !cond.first().is_truthy() {
                                return Ok(());
                            } else {
                                exec_block(block, &mut lua)?;
                            }
                        }
                        Err(err) => return Err(err),
                    }
                }
            }),
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
                // TODO use closing value (explist[3])
                lua.env.activate(|env| {
                    let mut lua = Interpreter::from(env);
                    let mut list = match eval_list(explist, &mut lua)? {
                        LuaResult::One(val) => vec![val],
                        LuaResult::Many(vals) => vals,
                    }; // [iter, state, initial value of control variable, closing value]
                    while list.len() < 4 {
                        list.push(Value::Nil);
                    }
                    let control_name = Value::String(namelist.first().unwrap().clone());
                    lua.env
                        .set_local(control_name.clone(), list.get(2).unwrap().clone());
                    let iter = list.first().unwrap();
                    loop {
                        let control = lua.env.get(&control_name);
                        let state = list.get(1).unwrap_or(&Value::Nil).clone();
                        let vals = iter.call(&mut lua, vec![state, control])?;
                        match vals {
                            LuaResult::One(val) => {
                                lua.env.set_local(control_name.clone(), val);
                            }
                            LuaResult::Many(res) => {
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
                        }
                        if let Value::Nil = lua.env.get(&control_name) {
                            return Ok(());
                        } else if let Err(err) = exec_block(block, &mut lua) {
                            return Err(err);
                        }
                    }
                })
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
                lua.env.activate(|env| {
                    let mut lua = Interpreter::from(env);
                    lua.env
                        .set_local(Value::String(name.clone()), start.clone());
                    loop {
                        let i = Expression::Variable(name.clone())
                            .eval(&mut lua)
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
                        if let Err(err) = exec_block(block, &mut lua) {
                            return Err(err);
                        }
                        let i = match i.add(&step) {
                            Ok(i) => i,
                            Err(e) => {
                                return Err(e);
                            }
                        };
                        lua.env.set_local(Value::String(name.clone()), i);
                    }
                    Ok(())
                })
            }
            Statement::If(exp, then_block, else_block) => lua.env.activate(|env| {
                let mut lua = Interpreter::from(env);
                if exp.eval(&mut lua)?.first().is_truthy() {
                    exec_block(then_block, &mut lua)
                } else {
                    exec_block(else_block, &mut lua)
                }
            }),
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

pub fn exec_block(block: &[Statement], lua: &mut Interpreter) -> Result<(), Exception> {
    // NOTE caller should pop the stack before propagating error
    for stat in block {
        stat.exec(lua)?;
    }
    Ok(())
}

pub fn eval_list(list: &[Expression], lua: &mut Interpreter) -> Result<LuaResult, Exception> {
    let count = list.len();
    if count == 0 {
        Ok(LuaResult::One(Value::Nil))
    } else {
        let mut vals = Vec::new();
        for exp in list.iter().take(count - 1) {
            vals.push(exp.eval(lua)?.first());
        }
        if let Some(last) = list.last() {
            match last.eval(lua)? {
                LuaResult::One(val) => vals.push(val),
                LuaResult::Many(mut many) => vals.append(&mut many),
            }
        }
        Ok(LuaResult::Many(vals))
    }
}
