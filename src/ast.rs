use crate::value::Float;

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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionBody(pub Vec<String>, pub Block);

impl FunctionBody {
    pub fn into_method(self) -> FunctionBody {
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
