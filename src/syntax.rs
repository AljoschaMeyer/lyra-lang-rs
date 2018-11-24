use std::path::PathBuf;
use std::sync::Arc;

use num::BigRational;

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

impl Meta {
    pub fn none() -> Meta {
        Meta {
            start: Position {
                offset: 0,
                line: 0,
                col: 0
            },
            source: Source(Arc::new(_Source::Interactive)),
        }
    }
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
    pub caught: Id,
    pub catcher: Box<Expression>,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord, Hash)]
pub struct Id(pub String, pub Meta);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Let {
    pub lhs: Id,
    pub rhs: Box<Expression>,
    pub continuing: Box<Expression>,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Rec {
    pub bindings: Vec<(Id, Fun)>,
    pub continuing: Box<Expression>,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Fun(pub Arc<_Fun>);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct _Fun {
    pub args: Vec<Id>,
    pub body: Box<Expression>,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct App {
    pub fun: Box<Expression>,
    pub args: Vec<Expression>,
    pub tail: bool,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Expression(pub _Expression, pub Meta);

impl Expression {
    pub fn untail(&mut self) {
        match self.0 {
            _Expression::App(App { ref mut tail, .. }) => *tail = false,
            _ => {}
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum _Expression {
    Nil,
    Args,
    Bool(bool),
    Int(BigRational),
    Char(char),
    String(String),
    Sequence(Vec<Expression>),
    Set(Set),
    Map(Vec<(Expression, Expression)>),
    Land(Box<Expression>, Box<Expression>),
    Lor(Box<Expression>, Box<Expression>),
    If(If),
    Throw(Throw),
    Try(Try),
    Id(Id),
    Let(Let),
    Rec(Rec),
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

impl From<(BigRational, Meta)> for Expression {
    fn from(d: (BigRational, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Int(val), meta)
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

impl From<Id> for Expression {
    fn from(val: Id) -> Expression {
        let meta = val.1.clone();
        Expression(_Expression::Id(val), meta)
    }
}

impl From<(Let, Meta)> for Expression {
    fn from(d: (Let, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Let(val), meta)
    }
}

impl From<(Rec, Meta)> for Expression {
    fn from(d: (Rec, Meta)) -> Expression {
        let (val, meta) = d;
        Expression(_Expression::Rec(val), meta)
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
