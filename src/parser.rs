use crate::ast::{Argument, BinaryOp, Block, Expression, FunctionBody, Statement, UnaryOp};
use crate::lex::{Category, Scanner, Token};
use crate::value::Float;

#[derive(Clone)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    current: Token,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>) -> Parser<'a> {
        let mut parser = Parser {
            scanner,
            current: Token::default(),
        };
        Parser::advance(&mut parser);
        parser
    }

    fn advance(&mut self) -> &Token {
        let current = self.scanner.next();
        self.current = current;
        self.current()
    }

    fn current(&self) -> &Token {
        &self.current
    }
}

#[derive(Debug)]
pub struct SyntaxError(pub &'static str);

pub fn parse(parser: &mut Parser) -> Result<Block, SyntaxError> {
    parse_chunk(parser)
}

fn parse_chunk(parser: &mut Parser) -> Result<Block, SyntaxError> {
    let block = parse_block(parser);
    assert_eq!(parser.current().category, Category::Eof, "<eof> expected");
    block
}

fn parse_block(parser: &mut Parser) -> Result<Block, SyntaxError> {
    // TODO Fix error recovery
    let mut stats = Vec::new();
    match parse_stat(parser) {
        Ok(stat) => {
            stats.push(stat);
            while let Ok(stat) = parse_stat(parser) {
                stats.push(stat);
            }
        }
        Err(SyntaxError(_)) => (), // TODO?
    }

    if let Ok(stat) = parse_retstat(parser) {
        stats.push(stat);
    }
    Ok(stats)
}

fn parse_stat(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    // TODO test
    let result = match parser.current().category {
        Category::SemiColon => {
            parser.advance();
            Ok(Statement::Empty)
        }
        Category::Break => {
            parser.advance();
            Ok(Statement::Break)
        }
        Category::Goto => {
            parser.advance();
            cont_goto(parser)
        }
        Category::Do => {
            parser.advance();
            parse_do(parser)
        }
        Category::While => {
            parser.advance();
            cont_while(parser)
        }
        Category::Repeat => {
            parser.advance();
            cont_repeat(parser)
        }
        Category::If => {
            parser.advance();
            cont_if(parser)
        }
        Category::For => {
            parser.advance();
            cont_for(parser)
        }
        Category::Function => {
            parser.advance();
            cont_function(parser)
        }
        Category::Local => {
            parser.advance();
            cont_local(parser)
        }
        _ => {
            if let Ok(stat) = parse_label(parser) {
                return Ok(stat);
            }
            let backup = parser.clone();
            if let Ok(varlist) = parse_varlist(parser) {
                if let Category::Equal = parser.current().category {
                    parser.advance();
                    let explist = parse_explist(parser)?;
                    return Ok(Statement::Assign(varlist, explist));
                } else {
                    return Err(SyntaxError("expected '='"));
                }
            } else {
                *parser = backup.clone();
                match parse_functioncall(parser) {
                    Ok(stat) => return Ok(stat),
                    Err(err) => {
                        *parser = backup;
                        return Err(err);
                    }
                }
            }
        }
    };
    if result.is_err() {
        println!(
            "{:?} {:?} in line {:?}",
            result,
            parser.current(),
            parser.scanner.line
        );
    }
    result
}

fn parse_retstat(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    if let Category::Return = parser.current().category {
        parser.advance();
    } else {
        return Err(SyntaxError("")); // TODO add error message? or pre-consume 'return'?
    }
    if let Category::SemiColon = parser.current().category {
        parser.advance();
        return Ok(Statement::Return(vec![Expression::Nil]));
    }
    let explist = if let Ok(explist) = parse_explist(parser) {
        explist
    } else {
        vec![Expression::Nil]
    };
    if let Category::SemiColon = parser.current().category {
        parser.advance();
    }
    Ok(Statement::Return(explist))
}

fn cont_goto(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    if let Category::Name(name) = &parser.current().category {
        let name = name.clone();
        parser.advance();
        Ok(Statement::Goto(name))
    } else {
        Err(SyntaxError("expected identifier"))
    }
}

fn parse_do(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    let block = parse_block(parser)?;
    if let Category::End = parser.current().category {
        parser.advance();
        Ok(Statement::Do(block))
    } else {
        Err(SyntaxError("expected 'end' (to close 'do')"))
    }
}

fn cont_while(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    // TODO test
    let exp = parse_exp(parser)?;
    if let Category::Do = parser.current().category {
        parser.advance();
        let block = parse_block(parser)?;
        if let Category::End = parser.current().category {
            parser.advance();
            Ok(Statement::While(exp, block))
        } else {
            Err(SyntaxError("expected 'end' (to close 'while')"))
        }
    } else {
        Err(SyntaxError("expected 'do'"))
    }
}

fn cont_repeat(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    // TODO test
    let block = parse_block(parser)?;
    if let Category::Until = parser.current().category {
        parser.advance();
        let exp = parse_exp(parser)?;
        let mut stat = block.clone();
        stat.push(Statement::While(exp, block));
        Ok(Statement::Do(stat))
    } else {
        Err(SyntaxError("expected 'until' (to close 'repeat')"))
    }
}

fn cont_if(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    let exp = parse_exp(parser)?;
    if let Category::Then = parser.current().category {
        parser.advance();
    } else {
        return Err(SyntaxError("expected 'then'"));
    }
    let then = parse_block(parser)?;
    let mut else_ifs = Vec::new();
    while let Category::ElseIf = parser.current().category {
        parser.advance();
        let exp = parse_exp(parser)?;
        if let Category::Then = parser.current().category {
            parser.advance();
        } else {
            return Err(SyntaxError("expected 'then'"));
        }
        let block = parse_block(parser)?;
        else_ifs.push((exp, block));
    }
    let mut else_block: Block = if let Category::Else = parser.current().category {
        parser.advance();
        parse_block(parser)?
    } else {
        Vec::new()
    };
    if let Category::End = parser.current().category {
        parser.advance();
    } else {
        return Err(SyntaxError("expected 'end' (to close 'if')"));
    }
    while !else_ifs.is_empty() {
        let (exp, block) = else_ifs.pop().unwrap();
        else_block = vec![Statement::If(exp, block, else_block)];
    }
    Ok(Statement::If(exp, then, else_block))
}

fn parse_namelist(parser: &mut Parser) -> Result<Vec<String>, SyntaxError> {
    let mut names = Vec::new();
    if let Category::Name(name) = &parser.current().category {
        names.push(name.clone());
        parser.advance();
    } else {
        return Err(SyntaxError("expected identifier"));
    }
    while let Category::Comma = parser.current().category {
        parser.advance();
        if let Category::Name(name) = &parser.current().category {
            names.push(name.clone());
            parser.advance();
        } else {
            return Err(SyntaxError("<name> expected"));
        }
    }
    Ok(names)
}

fn cont_for(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    // TODO
    let namelist = parse_namelist(parser)?;
    match parser.current().category {
        Category::Equal => {
            parser.advance();
            if namelist.len() != 1 {
                return Err(SyntaxError("")); // TODO add error message
            }
            let start = parse_exp(parser)?;
            if let Category::Comma = parser.current().category {
                parser.advance();
            } else {
                return Err(SyntaxError("expected ','"));
            }
            let end = parse_exp(parser)?;
            let step = if let Category::Comma = parser.current().category {
                parser.advance();
                parse_exp(parser)?
            } else {
                Expression::Integer(1)
            };
            if let Category::Do = parser.current().category {
                parser.advance();
            } else {
                return Err(SyntaxError("expected 'do'"));
            }
            let block = parse_block(parser)?;
            if let Category::End = parser.current().category {
                parser.advance();
                Ok(Statement::NumericalFor(
                    namelist[0].clone(),
                    start,
                    end,
                    step,
                    block,
                ))
            } else {
                Err(SyntaxError("expected 'end' (to close 'for')")) // expected end
            }
        }
        Category::In => {
            parser.advance();
            let explist = parse_explist(parser)?;
            if let Category::Do = parser.current().category {
                parser.advance();
                let block = parse_block(parser)?;
                if let Category::End = parser.current().category {
                    parser.advance();
                    Ok(Statement::GenericFor(namelist, explist, block))
                } else {
                    Err(SyntaxError("expected 'end' (to close 'for')"))
                }
            } else {
                Err(SyntaxError("expected 'do'"))
            }
        }
        _ => Err(SyntaxError("expected '=', or 'in'")), // or ','?
    }
}

fn cont_function(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    // TODO test
    match parse_funcname(parser) {
        Ok((func, is_method)) => {
            let body = parse_funcbody(parser)?;
            Ok(Statement::Assign(
                vec![func],
                vec![Expression::Function(if is_method {
                    body.into_method()
                } else {
                    body
                })],
            ))
        }
        Err(err) => Err(err),
    }
}

fn cont_local_function(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    //TODO test
    if let Category::Name(name) = &parser.current().category {
        let name = name.clone();
        parser.advance();
        let body = parse_funcbody(parser)?;
        Ok(Statement::Declare(
            vec![(name, None)],
            vec![Expression::Function(body)],
        ))
    } else {
        Err(SyntaxError("<name> expected"))
    }
}

fn parse_funcname(parser: &mut Parser) -> Result<(Expression, bool), SyntaxError> {
    // bool is true if function is a method
    let mut exp = if let Category::Name(name) = &parser.current().category {
        let name = name.clone();
        parser.advance();
        Expression::Variable(name)
    } else {
        return Err(SyntaxError("<name> expected"));
    };
    while let Category::Dot = parser.current().category {
        parser.advance();
        if let Category::Name(name) = &parser.current().category {
            exp = Expression::Index(Box::new(exp), Box::new(Expression::String(name.clone())));
            parser.advance();
        } else {
            return Err(SyntaxError("<name> expected"));
        }
    }
    if let Category::Colon = parser.current().category {
        parser.advance();
        if let Category::Name(name) = &parser.current().category {
            let name = name.clone();
            parser.advance();
            Ok((
                Expression::Index(Box::new(exp), Box::new(Expression::String(name))),
                true,
            ))
        } else {
            Err(SyntaxError("<name> expected"))
        }
    } else {
        Ok((exp, false))
    }
}

fn parse_exp(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    parse_exp_or(parser)
}

fn parse_exp_or(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let mut left = parse_exp_and(parser)?;
    while let Category::Or = parser.current().category {
        parser.advance();
        let right = parse_exp_and(parser)?;
        left = Expression::Binary(Box::new(left), BinaryOp::Or, Box::new(right));
    }
    Ok(left)
}

fn parse_exp_and(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let mut left = parse_exp_lessthan(parser)?;
    while let Category::And = parser.current().category {
        parser.advance();
        let right = parse_exp_lessthan(parser)?;
        left = Expression::Binary(Box::new(left), BinaryOp::And, Box::new(right));
    }
    Ok(left)
}

fn parse_exp_lessthan(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let mut left = parse_exp_pipe(parser)?;
    loop {
        let binop = match parser.current().category {
            Category::LessThan => BinaryOp::LessThan,
            Category::GreaterThan => BinaryOp::GreaterThan,
            Category::LessThanEqual => BinaryOp::LessThanEqual,
            Category::GreaterThanEqual => BinaryOp::GreaterThanEqual,
            Category::TildeEqual => BinaryOp::TildeEqual,
            Category::Equal2 => BinaryOp::Equal2,
            _ => break,
        };
        parser.advance();
        let right = parse_exp_pipe(parser)?;
        left = Expression::Binary(Box::new(left), binop, Box::new(right));
    }
    Ok(left)
}

fn parse_exp_pipe(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let mut left = parse_exp_tilde(parser)?;
    while let Category::Pipe = parser.current().category {
        parser.advance();
        let right = parse_exp_tilde(parser)?;
        left = Expression::Binary(Box::new(left), BinaryOp::Pipe, Box::new(right));
    }
    Ok(left)
}

fn parse_exp_tilde(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    // TODO test
    let mut left = parse_exp_ampersand(parser)?;
    while let Category::Tilde = parser.current().category {
        parser.advance();
        let right = parse_exp_ampersand(parser)?;
        left = Expression::Binary(Box::new(left), BinaryOp::Tilde, Box::new(right));
    }
    Ok(left)
}

fn parse_exp_ampersand(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let mut left = parse_exp_lessthan2(parser)?;
    while let Category::Ampersand = parser.current().category {
        parser.advance();
        let right = parse_exp_lessthan2(parser)?;
        left = Expression::Binary(Box::new(left), BinaryOp::Ampersand, Box::new(right));
    }
    Ok(left)
}

fn parse_exp_lessthan2(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let mut left = parse_exp_dot2(parser)?;
    loop {
        let binop = match parser.current().category {
            Category::LessThan2 => BinaryOp::LessThan2,
            Category::GreaterThan2 => BinaryOp::GreaterThan2,
            _ => break,
        };
        parser.advance();
        let right = parse_exp_dot2(parser)?;
        left = Expression::Binary(Box::new(left), binop, Box::new(right));
    }
    Ok(left)
}

fn parse_exp_dot2(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let left = parse_exp_plus(parser)?;
    if let Category::Dot2 = parser.current().category {
        parser.advance();
    } else {
        return Ok(left);
    }
    let right = parse_exp_dot2(parser)?;
    Ok(Expression::Binary(
        Box::new(left),
        BinaryOp::Dot2,
        Box::new(right),
    ))
}

fn parse_exp_plus(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let mut left = parse_exp_star(parser)?;
    loop {
        let binop = match parser.current().category {
            Category::Plus => BinaryOp::Plus,
            Category::Minus => BinaryOp::Minus,
            _ => break,
        };
        parser.advance();
        let right = parse_exp_star(parser)?;
        left = Expression::Binary(Box::new(left), binop, Box::new(right));
    }
    Ok(left)
}

fn parse_exp_star(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let mut left = parse_exp_unary(parser)?;
    loop {
        let binop = match parser.current().category {
            Category::Star => BinaryOp::Star,
            Category::Slash => BinaryOp::Slash,
            Category::Slash2 => BinaryOp::Slash2,
            Category::Percent => BinaryOp::Percent,
            _ => break,
        };
        parser.advance();
        let right = parse_exp_unary(parser)?;
        left = Expression::Binary(Box::new(left), binop, Box::new(right));
    }
    Ok(left)
}

fn parse_exp_unary(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let unop = match parser.current().category {
        Category::Not => Some(UnaryOp::Not),
        Category::Hash => Some(UnaryOp::Hash),
        Category::Minus => Some(UnaryOp::Minus),
        Category::Tilde => Some(UnaryOp::Tilde),
        _ => None,
    };
    if unop.is_none() {
        return parse_exp_caret(parser);
    }
    parser.advance();
    let exp = parse_exp_unary(parser)?;
    Ok(Expression::Unary(unop.unwrap(), Box::new(exp)))
}

fn parse_exp_caret(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let left = parse_exp_operand(parser)?;
    if let Category::Caret = parser.current().category {
        parser.advance();
    } else {
        return Ok(left);
    }
    let right = parse_exp_unary(parser)?;
    Ok(Expression::Binary(
        Box::new(left),
        BinaryOp::Caret,
        Box::new(right),
    ))
}

fn parse_exp_operand(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    match &parser.current().category {
        Category::Nil => {
            parser.advance();
            return Ok(Expression::Nil);
        }
        Category::False => {
            parser.advance();
            return Ok(Expression::Boolean(false));
        }
        Category::True => {
            parser.advance();
            return Ok(Expression::Boolean(true));
        }
        Category::Integer(value) => {
            let value = *value;
            parser.advance();
            return Ok(Expression::Integer(value));
        }
        Category::Float(value) => {
            let value = *value;
            parser.advance();
            return Ok(Expression::Float(Float(value)));
        }
        Category::String(value) => {
            let value = value.clone();
            parser.advance();
            return Ok(Expression::String(value));
        }
        Category::Dot3 => {
            parser.advance();
            return Ok(Expression::Dot3);
        }
        _ => {}
    }
    if let Ok(table) = parse_tableconstructor(parser) {
        Ok(Expression::Table(table))
    } else if let Ok(def) = parse_functiondef(parser) {
        Ok(def)
    } else if let Ok(prefix) = parse_prefixexp(parser) {
        Ok(prefix)
    } else {
        Err(SyntaxError("unexpected symbol"))
    }
}

fn parse_explist(parser: &mut Parser) -> Result<Vec<Expression>, SyntaxError> {
    let mut exps = vec![parse_exp(parser)?];
    while let Category::Comma = parser.current().category {
        parser.advance();
        exps.push(parse_exp(parser)?);
    }
    Ok(exps)
}

fn parse_funcbody(parser: &mut Parser) -> Result<FunctionBody, SyntaxError> {
    if let Category::LeftParen = parser.current().category {
        parser.advance();
    } else {
        return Err(SyntaxError("'(' expected"));
    }
    let parlist = if let Category::RightParen = parser.current().category {
        parser.advance();
        vec![]
    } else {
        match parse_parlist(parser) {
            Ok(parlist) => {
                if let Category::RightParen = parser.current().category {
                    parser.advance();
                } else {
                    return Err(SyntaxError("expected ')'"));
                }
                parlist
            }
            Err(err) => return Err(err),
        }
    };
    let block = parse_block(parser)?; // TODO test parse_block (stack overflow)
    if let Category::End = parser.current().category {
        parser.advance();
    } else {
        return Err(SyntaxError("expected 'end' (to close 'function')"));
    }
    Ok(FunctionBody(parlist, block))
}

fn parse_parlist(parser: &mut Parser) -> Result<Vec<String>, SyntaxError> {
    if let Category::Dot3 = parser.current().category {
        parser.advance();
        Ok(vec![String::from("...")])
    } else {
        let mut names = Vec::new();
        if let Category::Name(name) = &parser.current().category {
            names.push(name.clone());
            parser.advance();
        } else {
            return Err(SyntaxError("expected argument"));
        }
        while let Category::Comma = &parser.current().category {
            parser.advance();
            match &parser.current().category {
                Category::Name(name) => {
                    names.push(name.clone());
                    parser.advance();
                }
                Category::Dot3 => {
                    parser.advance();
                    names.push("...".to_string());
                    break;
                }
                _ => {
                    return Err(SyntaxError("expected argument"));
                }
            }
        }
        Ok(names)
    }
}

fn parse_varlist(parser: &mut Parser) -> Result<Vec<Expression>, SyntaxError> {
    let mut vars = vec![parse_var(parser)?];
    while let Category::Comma = parser.current().category {
        parser.advance();
        vars.push(parse_var(parser)?);
    }
    Ok(vars)
}

fn parse_functioncall(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    match parse_prefixexp(parser) {
        Ok(Expression::FunctionCall(func, args)) => Ok(Statement::FunctionCall(*func, args)),
        Err(err) => Err(err), // expected function or method call
        _ => Err(SyntaxError("expected statement")),
    }
}

fn parse_label(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    if let Category::Colon2 = parser.current().category {
        parser.advance();
    } else {
        return Err(SyntaxError("")); // TODO add error message? or pre-consume ::?
    }
    let name = if let Category::Name(name) = &parser.current().category {
        name.clone()
    } else {
        return Err(SyntaxError("expected identifier"));
    };
    parser.advance();

    if let Category::Colon2 = parser.current().category {
        parser.advance();
        Ok(Statement::Label(name))
    } else {
        Err(SyntaxError("expected '::"))
    }
}

fn parse_prefixexp(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    let mut prefix = match &parser.current().category {
        Category::LeftParen => {
            parser.advance();
            let exp = parse_exp(parser)?;
            if let Category::RightParen = parser.current().category {
                parser.advance();
                exp
            } else {
                return Err(SyntaxError("expected expression"));
            }
        }
        Category::Name(name) => {
            let name = name.clone();
            parser.advance();
            Expression::Variable(name)
        }
        _ => {
            return Err(SyntaxError("")); // TODO is this reachable?
        }
    };
    loop {
        match parser.current().category {
            Category::LeftBracket => {
                parser.advance();
                let exp = parse_exp(parser)?;
                if let Category::RightBracket = parser.current().category {
                    parser.advance();
                    prefix = Expression::Index(Box::new(prefix), Box::new(exp))
                } else {
                    return Err(SyntaxError("expected expression"));
                }
            }
            Category::Dot => {
                parser.advance();
                if let Category::Name(name) = &parser.current().category {
                    let name = name.clone();
                    parser.advance();
                    prefix =
                        Expression::Index(Box::new(prefix), Box::new(Expression::String(name)));
                } else {
                    return Err(SyntaxError("expected identifier"));
                }
            }
            Category::Colon => {
                parser.advance();
                if let Category::Name(name) = &parser.current().category {
                    let name = name.clone();
                    parser.advance();
                    let mut args = parse_args(parser)?;
                    args.insert(0, prefix.clone());
                    prefix = Expression::FunctionCall(
                        Box::new(Expression::Index(
                            Box::new(prefix),
                            Box::new(Expression::String(name)),
                        )),
                        args,
                    );
                }
            }
            _ => {
                if let Ok(args) = parse_args(parser) {
                    prefix = Expression::FunctionCall(Box::new(prefix), args);
                } else {
                    return Ok(prefix);
                }
            }
        }
    }
}

fn parse_args(parser: &mut Parser) -> Result<Argument, SyntaxError> {
    match &parser.current().category {
        Category::LeftParen => {
            parser.advance();
            let explist = if let Ok(explist) = parse_explist(parser) {
                explist
            } else {
                vec![]
            };
            if let Category::RightParen = parser.current().category {
                parser.advance();
            } else {
                return Err(SyntaxError("expected ')' (to close '(')"));
            }
            Ok(explist)
        }
        Category::String(value) => {
            let value = value.clone();
            parser.advance();
            Ok(vec![Expression::String(value)])
        }
        Category::LeftBrace => {
            let table = parse_tableconstructor(parser)?;
            Ok(vec![Expression::Table(table)])
        }
        _ => Err(SyntaxError("unexpected symbol")),
    }
}

fn parse_var(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    match parse_prefixexp(parser) {
        Ok(Expression::Variable(name)) => Ok(Expression::Variable(name)),
        Ok(Expression::Index(a, b)) => Ok(Expression::Index(a, b)),
        _ => Err(SyntaxError("expected identifier or field")),
    }
}

fn parse_tableconstructor(
    parser: &mut Parser,
) -> Result<Vec<(Expression, Expression)>, SyntaxError> {
    if let Category::LeftBrace = parser.current().category {
        parser.advance();
    } else {
        return Err(SyntaxError("")); // TODO add error message? or pre-consume '{'?
    }
    if let Category::RightBrace = parser.current().category {
        parser.advance();
        return Ok(vec![]);
    }
    let result = parse_fieldlist(parser)?;
    if let Category::RightBrace = parser.current().category {
        parser.advance();
        Ok(result)
    } else {
        Err(SyntaxError("expected '}' (to close '{')"))
    }
}

fn parse_fieldlist(parser: &mut Parser) -> Result<Vec<(Expression, Expression)>, SyntaxError> {
    let mut fields = Vec::new();
    let mut index = 1;

    let Field(key, val) = parse_field(parser)?;
    if let Some(key) = key {
        fields.push((key, val));
    } else {
        fields.push((Expression::Integer(index), val));
        index += 1;
    }
    while parse_fieldsep(parser).is_ok() {
        let Field(key, val) = if let Ok(field) = parse_field(parser) {
            field
        } else {
            break;
        };
        if let Some(key) = key {
            fields.push((key, val));
        } else {
            fields.push((Expression::Integer(index), val));
            index += 1;
        }
    }
    Ok(fields)
}

fn parse_fieldsep(parser: &mut Parser) -> Result<Category, SyntaxError> {
    match parser.current().category {
        Category::Comma => {
            parser.advance();
            Ok(Category::Comma)
        }
        Category::SemiColon => {
            parser.advance();
            Ok(Category::SemiColon)
        }
        _ => Err(SyntaxError("expected '}' (to close '{')")),
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Field(pub Option<Expression>, pub Expression); // key, value pair

fn parse_field(parser: &mut Parser) -> Result<Field, SyntaxError> {
    if let Category::LeftBracket = parser.current().category {
        parser.advance();
        let left = parse_exp(parser)?;
        if let Category::RightBracket = parser.current().category {
            parser.advance();
        } else {
            return Err(SyntaxError("']' expected"));
        }
        if let Category::Equal = parser.current().category {
            parser.advance();
        } else {
            return Err(SyntaxError("expected '='"));
        }
        let right = parse_exp(parser)?;
        Ok(Field(Some(left), right))
    } else if let Ok(exp) = parse_exp(parser) {
        if let Expression::Variable(name) = exp {
            if let Category::Equal = parser.current().category {
                parser.advance();
                let exp = parse_exp(parser)?;
                Ok(Field(Some(Expression::String(name)), exp))
            } else {
                Ok(Field(None, Expression::Variable(name)))
            }
        } else {
            Ok(Field(None, exp))
        }
    } else {
        Err(SyntaxError("expected expression"))
    }
}

fn parse_functiondef(parser: &mut Parser) -> Result<Expression, SyntaxError> {
    if let Category::Function = parser.current().category {
        parser.advance();
        Ok(Expression::Function(parse_funcbody(parser)?)) // TODO Test funcbody
    } else {
        Err(SyntaxError("")) // TODO add error message? or pre-consume 'function'?
    }
}

fn cont_local(parser: &mut Parser) -> Result<Statement, SyntaxError> {
    if let Category::Function = parser.current().category {
        parser.advance();
        cont_local_function(parser)
    } else if let Ok(attnamelist) = parse_attnamelist(parser) {
        if let Category::Equal = parser.current().category {
            parser.advance();
            let explist = parse_explist(parser)?;
            Ok(Statement::Declare(attnamelist, explist))
        } else {
            Ok(Statement::Declare(attnamelist, Vec::new()))
        }
    } else {
        Err(SyntaxError("expected identifier"))
    }
}

fn parse_attnamelist(parser: &mut Parser) -> Result<Vec<(String, Option<String>)>, SyntaxError> {
    let mut attnames = Vec::new();
    if let Category::Name(name) = &parser.current().category {
        let name = name.clone();
        parser.advance();

        let attrib = parse_attrib(parser)?;
        attnames.push((name, attrib));
        while let Category::Comma = parser.current().category {
            parser.advance();
            if let Category::Name(name) = &parser.current().category {
                let name = name.clone();
                parser.advance();
                let attrib = parse_attrib(parser)?;
                attnames.push((name, attrib));
            } else {
                return Err(SyntaxError("expected identifier"));
            }
        }
        Ok(attnames)
    } else {
        Err(SyntaxError("expected identifier"))
    }
}

fn parse_attrib(parser: &mut Parser) -> Result<Option<String>, SyntaxError> {
    if let Category::LessThan = parser.current().category {
        parser.advance();
    } else {
        return Ok(None);
    }
    if let Category::Name(name) = &parser.current().category {
        let name = name.clone();
        parser.advance();
        if let Category::GreaterThan = parser.current().category {
            parser.advance();
            Ok(Some(name))
        } else {
            Err(SyntaxError("expected '>' (to close '<')"))
        }
    } else {
        Err(SyntaxError("expected identifier"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::{Category, Scanner};
    use crate::value::Float;
    use std::fmt::Debug;

    fn check_parse<T: Debug + PartialEq>(
        f: fn(&mut Parser) -> Result<T, SyntaxError>,
        src: &str,
        expected: T,
    ) {
        let mut p = Parser::new(Scanner::new(&src));
        if let Ok(out) = f(&mut p) {
            assert_eq!(out, expected);
        } else {
            assert!(false, format!("expected {:?} but got Err", expected));
        }
    }

    #[test]
    fn fieldsep() {
        let sources = vec![",", ";"];
        let outs = vec![Category::Comma, Category::SemiColon];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_fieldsep, source, out);
        }
    }

    #[test]
    fn namelist() {
        let sources = vec!["foo", "foo, bar", "foo, bar, baz"];
        let outs = vec![
            vec![String::from("foo")],
            vec![String::from("foo"), String::from("bar")],
            vec![
                String::from("foo"),
                String::from("bar"),
                String::from("baz"),
            ],
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_namelist, source, out);
        }
    }

    #[test]
    fn parlist() {
        let sources = vec!["...", "foo", "foo, bar", "foo, bar, ..."];
        let outs = vec![
            vec![String::from("...")],
            vec![String::from("foo")],
            vec![String::from("foo"), String::from("bar")],
            vec![
                String::from("foo"),
                String::from("bar"),
                String::from("..."),
            ],
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_parlist, source, out);
        }
    }

    #[test]
    fn funcname() {
        let sources = vec!["foo", "foo.bar", "foo.bar.baz", "foo:baz", "foo.bar:baz"];
        let outs = vec![
            (Expression::Variable("foo".to_string()), false),
            (
                Expression::Index(
                    Box::new(Expression::Variable("foo".to_string())),
                    Box::new(Expression::String("bar".to_string())),
                ),
                false,
            ),
            (
                Expression::Index(
                    Box::new(Expression::Index(
                        Box::new(Expression::Variable("foo".to_string())),
                        Box::new(Expression::String("bar".to_string())),
                    )),
                    Box::new(Expression::String("baz".to_string())),
                ),
                false,
            ),
            (
                Expression::Index(
                    Box::new(Expression::Variable("foo".to_string())),
                    Box::new(Expression::String("baz".to_string())),
                ),
                true,
            ),
            (
                Expression::Index(
                    Box::new(Expression::Index(
                        Box::new(Expression::Variable("foo".to_string())),
                        Box::new(Expression::String("bar".to_string())),
                    )),
                    Box::new(Expression::String("baz".to_string())),
                ),
                true,
            ),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_funcname, source, out);
        }
    }

    #[test]
    fn label() {
        let sources = vec![":: foo ::", "::foo::"];
        let outs = vec![
            Statement::Label("foo".to_string()),
            Statement::Label("foo".to_string()),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_label, source, out);
        }
    }

    #[test]
    fn attrib() {
        let sources = vec!["", "<const>", "<close>"];
        let outs = vec![None, Some("const".to_string()), Some("close".to_string())];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_attrib, source, out);
        }
    }

    #[test]
    fn attnamelist() {
        let sources = vec![
            "foo",
            "foo, bar",
            "foo<const>",
            "foo<const>, bar",
            "foo, bar<close>",
            "foo<const>, bar<close>",
            "foo<const>, bar<const>, baz<const>",
        ];
        let outs = vec![
            vec![("foo".to_string(), None)],
            vec![("foo".to_string(), None), ("bar".to_string(), None)],
            vec![("foo".to_string(), Some("const".to_string()))],
            vec![
                ("foo".to_string(), Some("const".to_string())),
                ("bar".to_string(), None),
            ],
            vec![
                ("foo".to_string(), None),
                ("bar".to_string(), Some("close".to_string())),
            ],
            vec![
                ("foo".to_string(), Some("const".to_string())),
                ("bar".to_string(), Some("close".to_string())),
            ],
            vec![
                ("foo".to_string(), Some("const".to_string())),
                ("bar".to_string(), Some("const".to_string())),
                ("baz".to_string(), Some("const".to_string())),
            ],
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_attnamelist, source, out);
        }
    }

    #[test]
    fn funcbody() {
        let sources = vec!["() end"];
        let outs = vec![FunctionBody(vec![], vec![])];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_funcbody, source, out);
        }
    }

    #[test]
    fn retstat() {
        let sources = vec![
            "return",
            "return;",
            "return nil",
            "return 1, 2",
            "return 1, 2;",
        ];
        let outs = vec![
            Statement::Return(vec![Expression::Nil]),
            Statement::Return(vec![Expression::Nil]),
            Statement::Return(vec![Expression::Nil]),
            Statement::Return(vec![Expression::Integer(1), Expression::Integer(2)]),
            Statement::Return(vec![Expression::Integer(1), Expression::Integer(2)]),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_retstat, source, out);
        }
    }

    #[test]
    fn exp() {
        let sources = vec![
            "nil",
            "false",
            "true",
            "4",
            "4.0",
            "4e-1",
            "\"Hello, world!\"",
            "\'Hello, world!\'",
            "...",
            "function() end",
            "(nil)",
            "{a, b = 1, [c] = 2,}",
            "1 + 1",
            "-2",
        ];
        let outs = vec![
            Expression::Nil,
            Expression::Boolean(false),
            Expression::Boolean(true),
            Expression::Integer(4),
            Expression::Float(Float(4.0)),
            Expression::Float(Float(0.4)),
            Expression::String("Hello, world!".to_string()),
            Expression::String("Hello, world!".to_string()),
            Expression::Dot3,
            Expression::Function(FunctionBody(vec![], vec![])),
            Expression::Nil,
            Expression::Table(vec![
                (
                    Expression::Integer(1),
                    Expression::Variable("a".to_string()),
                ),
                (Expression::String("b".to_string()), Expression::Integer(1)),
                (
                    Expression::Variable("c".to_string()),
                    Expression::Integer(2),
                ),
            ]),
            Expression::Binary(
                Box::new(Expression::Integer(1)),
                BinaryOp::Plus,
                Box::new(Expression::Integer(1)),
            ),
            Expression::Unary(UnaryOp::Minus, Box::new(Expression::Integer(2))),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_exp, source, out);
        }
    }

    #[test]
    fn var() {
        let sources = vec!["foo", "foo[bar]", "foo.bar"];
        let outs = vec![
            Expression::Variable("foo".to_string()),
            Expression::Index(
                Box::new(Expression::Variable("foo".to_string())),
                Box::new(Expression::Variable("bar".to_string())),
            ),
            Expression::Index(
                Box::new(Expression::Variable("foo".to_string())),
                Box::new(Expression::String("bar".to_string())),
            ),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_var, source, out);
        }
    }

    #[test]
    fn prefixexp() {
        let sources = vec!["hello", "(hello)", "hello()"];
        let outs = vec![
            Expression::Variable("hello".to_string()),
            Expression::Variable("hello".to_string()),
            Expression::FunctionCall(Box::new(Expression::Variable("hello".to_string())), vec![]),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_prefixexp, source, out);
        }
    }

    #[test]
    fn field() {
        let sources = vec!["[a] = 1", "a = 1", "a", "1"];
        let outs = vec![
            Field(
                Some(Expression::Variable("a".to_string())),
                Expression::Integer(1),
            ),
            Field(
                Some(Expression::String("a".to_string())),
                Expression::Integer(1),
            ),
            Field(None, Expression::Variable("a".to_string())),
            Field(None, Expression::Integer(1)),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_field, source, out);
        }
    }

    #[test]
    fn fieldlist() {
        let sources = vec!["a,", "[a] = 1", "a, b", "a; b = 1"];
        let outs = vec![
            vec![(
                Expression::Integer(1),
                Expression::Variable("a".to_string()),
            )],
            vec![(
                Expression::Variable("a".to_string()),
                Expression::Integer(1),
            )],
            vec![
                (
                    Expression::Integer(1),
                    Expression::Variable("a".to_string()),
                ),
                (
                    Expression::Integer(2),
                    Expression::Variable("b".to_string()),
                ),
            ],
            vec![
                (
                    Expression::Integer(1),
                    Expression::Variable("a".to_string()),
                ),
                (Expression::String("b".to_string()), Expression::Integer(1)),
            ],
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_fieldlist, source, out);
        }
    }

    #[test]
    fn tableconstructor() {
        let sources = vec!["{}", "{a}", "{a = 1, [a] = 1,}"];
        let outs = vec![
            vec![],
            vec![(
                Expression::Integer(1),
                Expression::Variable("a".to_string()),
            )],
            vec![
                (Expression::String("a".to_string()), Expression::Integer(1)),
                (
                    Expression::Variable("a".to_string()),
                    Expression::Integer(1),
                ),
            ],
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_tableconstructor, source, out);
        }
    }

    #[test]
    fn stat() {
        // TODO test label
        let sources = vec![
            "break",
            "goto label",
            "do end",
            "while false do end",
            "repeat until false",
            "for i = 10, 1, -1 do print(i) end",
            // "for i, a in ipairs(t) do print(i, a) end"
            // "function funcname funcbody",
            // "local function Name funcbody",
            "local foo<const>, bar, baz<close>",
            "local foo, bar = 1, 2",
            "f()",
            "if a then f() elseif b then g() else h() end",
        ];
        let outs = vec![
            Statement::Break,
            Statement::Goto("label".to_string()),
            Statement::Do(vec![]),
            Statement::While(Expression::Boolean(false), vec![]),
            Statement::Do(vec![Statement::While(Expression::Boolean(false), vec![])]),
            Statement::NumericalFor(
                "i".to_string(),
                Expression::Integer(10),
                Expression::Integer(1),
                Expression::Unary(UnaryOp::Minus, Box::new(Expression::Integer(1))),
                vec![Statement::FunctionCall(
                    Expression::Variable("print".to_string()),
                    vec![Expression::Variable("i".to_string())],
                )],
            ),
            /*
            Statement::GenericFor(

            ),
            */
            Statement::Declare(
                vec![
                    ("foo".to_string(), Some("const".to_string())),
                    ("bar".to_string(), None),
                    ("baz".to_string(), Some("close".to_string())),
                ],
                vec![],
            ),
            Statement::Declare(
                vec![("foo".to_string(), None), ("bar".to_string(), None)],
                vec![Expression::Integer(1), Expression::Integer(2)],
            ),
            Statement::FunctionCall(Expression::Variable("f".to_string()), vec![]),
            Statement::If(
                Expression::Variable("a".to_string()),
                vec![Statement::FunctionCall(
                    Expression::Variable("f".to_string()),
                    vec![],
                )],
                vec![Statement::If(
                    Expression::Variable("b".to_string()),
                    vec![Statement::FunctionCall(
                        Expression::Variable("g".to_string()),
                        vec![],
                    )],
                    vec![Statement::FunctionCall(
                        Expression::Variable("h".to_string()),
                        vec![],
                    )],
                )],
            ),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_stat, source, out);
        }
    }

    #[test]
    fn explist() {
        let sources = vec!["a", "a, b"];
        let outs = vec![
            vec![Expression::Variable("a".to_string())],
            vec![
                Expression::Variable("a".to_string()),
                Expression::Variable("b".to_string()),
            ],
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_explist, source, out);
        }
    }

    #[test]
    fn functioncall() {
        let sources = vec!["foo(bar)", "foo:bar(baz)"];
        let outs = vec![
            Statement::FunctionCall(
                Expression::Variable("foo".to_string()),
                vec![Expression::Variable("bar".to_string())],
            ),
            Statement::FunctionCall(
                Expression::Index(
                    Box::new(Expression::Variable("foo".to_string())),
                    Box::new(Expression::String("bar".to_string())),
                ),
                vec![
                    Expression::Variable("foo".to_string()),
                    Expression::Variable("baz".to_string()),
                ],
            ),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_functioncall, source, out);
        }
    }

    #[test]
    fn args() {
        let sources = vec!["()", "(1)", "\"foobar\"", "{}", "{1, a = 1, [a] = 1}"];
        let outs = vec![
            vec![],
            vec![Expression::Integer(1)],
            vec![Expression::String("foobar".to_string())],
            vec![Expression::Table(Vec::new())],
            vec![Expression::Table(vec![
                (Expression::Integer(1), Expression::Integer(1)),
                (Expression::String("a".to_string()), Expression::Integer(1)),
                (
                    Expression::Variable("a".to_string()),
                    Expression::Integer(1),
                ),
            ])],
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_args, source, out);
        }
    }

    #[test]
    fn varlist() {
        let sources = vec!["foo", "foo, bar", "foo, bar, baz"];
        let outs = vec![
            vec![Expression::Variable("foo".to_string())],
            vec![
                Expression::Variable("foo".to_string()),
                Expression::Variable("bar".to_string()),
            ],
            vec![
                Expression::Variable("foo".to_string()),
                Expression::Variable("bar".to_string()),
                Expression::Variable("baz".to_string()),
            ],
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_varlist, source, out);
        }
    }

    #[test]
    fn block() {
        let sources = vec![""];
        let outs = vec![vec![]];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_block, source, out);
        }
    }

    #[test]
    fn exp_operand() {
        let sources = vec!["nil", "false", "true", "1", "1.0", "\"hello\"", "..."];
        let outs = vec![
            Expression::Nil,
            Expression::Boolean(false),
            Expression::Boolean(true),
            Expression::Integer(1),
            Expression::Float(Float(1.0)),
            Expression::String("hello".to_string()),
            Expression::Dot3,
            // tableconstructor
            // functiondef
            // prefix
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_exp_operand, source, out);
        }
    }

    #[test]
    fn exp_caret() {
        let sources = vec!["2", "2^3"];
        let outs = vec![
            Expression::Integer(2),
            Expression::Binary(
                Box::new(Expression::Integer(2)),
                BinaryOp::Caret,
                Box::new(Expression::Integer(3)),
            ),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_exp_caret, source, out);
        }
    }

    #[test]
    fn exp_unary() {
        let sources = vec!["2", "2^3", "-2", "-2^3"];
        let outs = vec![
            Expression::Integer(2),
            Expression::Binary(
                Box::new(Expression::Integer(2)),
                BinaryOp::Caret,
                Box::new(Expression::Integer(3)),
            ),
            Expression::Unary(UnaryOp::Minus, Box::new(Expression::Integer(2))),
            Expression::Unary(
                UnaryOp::Minus,
                Box::new(Expression::Binary(
                    Box::new(Expression::Integer(2)),
                    BinaryOp::Caret,
                    Box::new(Expression::Integer(3)),
                )),
            ),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_exp_unary, source, out);
        }
    }

    #[test]
    fn exp_star() {
        let sources = vec!["1", "1 * 1", "1 / 1", "2 % 1"];
        let outs = vec![
            Expression::Integer(1),
            Expression::Binary(
                Box::new(Expression::Integer(1)),
                BinaryOp::Star,
                Box::new(Expression::Integer(1)),
            ),
            Expression::Binary(
                Box::new(Expression::Integer(1)),
                BinaryOp::Slash,
                Box::new(Expression::Integer(1)),
            ),
            Expression::Binary(
                Box::new(Expression::Integer(2)),
                BinaryOp::Percent,
                Box::new(Expression::Integer(1)),
            ),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_exp_star, source, out);
        }
    }

    #[test]
    fn exp_plus() {
        let sources = vec!["1", "1 + 1", "1 - 1"];
        let outs = vec![
            Expression::Integer(1),
            Expression::Binary(
                Box::new(Expression::Integer(1)),
                BinaryOp::Plus,
                Box::new(Expression::Integer(1)),
            ),
            Expression::Binary(
                Box::new(Expression::Integer(1)),
                BinaryOp::Minus,
                Box::new(Expression::Integer(1)),
            ),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_exp_plus, source, out);
        }
    }

    #[test]
    fn exp_dot2() {
        let sources = vec!["\"foo\"", "\"foo\" .. \"bar\""];
        let outs = vec![
            Expression::String("foo".to_string()),
            Expression::Binary(
                Box::new(Expression::String("foo".to_string())),
                BinaryOp::Dot2,
                Box::new(Expression::String("bar".to_string())),
            ),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_exp_dot2, source, out);
        }
    }

    #[test]
    fn exp_lessthan2() {
        let sources = vec!["1 + 1", "1 << 2", "1 >> 2"];
        let outs = vec![
            Expression::Binary(
                Box::new(Expression::Integer(1)),
                BinaryOp::Plus,
                Box::new(Expression::Integer(1)),
            ),
            Expression::Binary(
                Box::new(Expression::Integer(1)),
                BinaryOp::LessThan2,
                Box::new(Expression::Integer(2)),
            ),
            Expression::Binary(
                Box::new(Expression::Integer(1)),
                BinaryOp::GreaterThan2,
                Box::new(Expression::Integer(2)),
            ),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_exp_lessthan2, source, out);
        }
    }

    #[test]
    fn exp_ampersand() {
        let sources = vec!["1 >> 2", "7 & 8"];
        let outs = vec![
            Expression::Binary(
                Box::new(Expression::Integer(1)),
                BinaryOp::GreaterThan2,
                Box::new(Expression::Integer(2)),
            ),
            Expression::Binary(
                Box::new(Expression::Integer(7)),
                BinaryOp::Ampersand,
                Box::new(Expression::Integer(8)),
            ),
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_exp_ampersand, source, out);
        }
    }

    /*
    #[test]
    fn() {
        let sources = vec![
        ];
        let outs = vec![
        ];
        for (source, out) in sources.iter().zip(outs) {
            check_parse(parse_, source, out);
        }
    }
    */
}
