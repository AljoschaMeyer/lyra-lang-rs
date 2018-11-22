use std::path::PathBuf;
use std::sync::Arc;

use either::{Either, Left, Right};
use num::BigRational;

use super::CmpF64;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub col: usize,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub enum _Source {
    File(PathBuf),
    Eval(Meta),
    Interactive,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub struct Source(pub Arc<_Source>);

#[derive(Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub struct Meta {
    pub start: Position,
    pub source: Source,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Set(pub Vec<Expression>);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct If {
    pub cond: Box<Expression>,
    pub then: Box<Expression>,
    pub else_: Option<Box<Expression>>,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Throw(pub Box<Expression>);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Try {
    pub to_try: Box<Expression>,
    pub caught: Pattern,
    pub catcher: Box<Expression>,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Pause {
    pub cond: Option<Box<Expression>>,
    pub continuing: Box<Expression>,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord, Hash)]
pub struct Id(pub String);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Let {
    pub lhs: Pattern,
    pub rhs: Box<Expression>,
    pub continuing: Box<Expression>,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum Pattern {
    Id(Id, Meta),
    Map(Vec<(Id, Meta)>),
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Fun(pub Arc<_Fun>);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct _Fun {
    pub args: Vec<(Id, Meta)>,
    pub body: Box<Expression>,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct App {
    pub fun: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Expression(pub _Expression, pub Meta);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum _Expression {
    Nil,
    Args,
    Bool(bool),
    Int(BigRational),
    Float(CmpF64),
    Char(char),
    String(String),
    Sequence(Vec<Expression>),
    Set(Set),
    Map(Vec<(Expression, Expression)>),
    If(If),
    Throw(Throw),
    Try(Try),
    Pause(Pause),
    Id(Id),
    Let(Let),
    Fun(Fun),
    App(App)
}

impl From<((), Meta)> for Expression {
    fn from(d: ((), Meta)) -> Expression {
        let (_val, meta) = d;
        Expression(_Expression::Nil, meta)
    }
}

impl From<(bool, Meta)> for Expression {
    fn from(d: (bool, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Bool(val), meta)
    }
}

impl From<(char, Meta)> for Expression {
    fn from(d: (char, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Char(val), meta)
    }
}

impl From<(String, Meta)> for Expression {
    fn from(d: (String, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::String(val), meta)
    }
}

impl From<(Either<BigRational, f64>, Meta)> for Expression {
    fn from(d: (Either<BigRational, f64>, Meta)) -> Expression {
        let (val, meta) = d;
        match val {
            Left(int) => Expression(_Expression::Int(int), meta),
            Right(float) => Expression(_Expression::Float(CmpF64(float)), meta),
        }
    }
}

impl From<(Vec<Expression>, Meta)> for Expression {
    fn from(d: (Vec<Expression>, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Sequence(val), meta)
    }
}

impl From<(Set, Meta)> for Expression {
    fn from(d: (Set, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Set(val), meta)
    }
}

impl From<(Vec<(Expression, Expression)>, Meta)> for Expression {
    fn from(d: (Vec<(Expression, Expression)>, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Map(val), meta)
    }
}

impl From<(If, Meta)> for Expression {
    fn from(d: (If, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::If(val), meta)
    }
}

impl From<(Throw, Meta)> for Expression {
    fn from(d: (Throw, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Throw(val), meta)
    }
}

impl From<(Try, Meta)> for Expression {
    fn from(d: (Try, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Try(val), meta)
    }
}

impl From<(Pause, Meta)> for Expression {
    fn from(d: (Pause, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Pause(val), meta)
    }
}

impl From<(Id, Meta)> for Expression {
    fn from(d: (Id, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Id(val), meta)
    }
}

impl From<(Let, Meta)> for Expression {
    fn from(d: (Let, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Let(val), meta)
    }
}

impl From<(Fun, Meta)> for Expression {
    fn from(d: (Fun, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Fun(val), meta)
    }
}

impl From<(App, Meta)> for Expression {
    fn from(d: (App, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::App(val), meta)
    }
}
