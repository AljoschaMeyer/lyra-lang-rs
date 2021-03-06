use std::path::PathBuf;
use std::rc::Rc;

use ordered_float::OrderedFloat;
use rug::Integer;

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
    Other,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub struct Source(pub Rc<_Source>);

impl Source {
    /// Create a new source that doesn't carry any information.
    pub fn other() -> Source {
        Source(Rc::new(_Source::Other))
    }
}

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
            source: Source(Rc::new(_Source::Other)),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Expression(pub _Expression, pub Meta);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum _Expression {
    Id(String),
    Nil,
    Bool(bool),
    Int(Integer),
    Float(OrderedFloat<f64>),
    Land(Box<Expression>, Box<Expression>),
    Lor(Box<Expression>, Box<Expression>),
    BinOp(Box<Expression>, BinOp, Box<Expression>),
    Not(Box<Expression>),
    If(Box<Expression>, Box<Option<Statement>>, Box<Option<Statement>>),
    While(Box<Expression>, Box<Option<Statement>>),
    Try(Box<Option<Statement>>, Pattern, Box<Option<Statement>>),
    Case(Box<Expression>, Box<[(Patterns, Box<Option<Statement>>)]>),
    Loop(Box<Expression>, Box<[(Patterns, Box<Option<Statement>>)]>),
    Application(Box<Expression>, Box<[Expression]>),
    Fun(FunLiteral),

    // operators, literals, for (?), map access, indexing
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum BinOp {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Add, // TODO all the other numeric ops
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct FunLiteral(pub Box<[Pattern]>, pub Rc<Option<Statement>>);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Statement(pub _Statement, pub Meta);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum _Statement {
    Chain(Box<Statement>, Box<Statement>),
    Exp(Expression),
    Let(Pattern, Expression),
    Assign(String, Expression),
    Throw(Expression),
    Return(Expression),
    Break(Expression),
    Rec(Box<[(String, FunLiteral)]>),

    // let await?, await for?
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Pattern(pub _Pattern, pub Meta);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum _Pattern {
    Blank,
    Id {
        id: String,
        mutable: bool,
    },
    Nil,
    Bool(bool),
    Int(Integer),
    Float(OrderedFloat<f64>),
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Patterns(pub Box<[Pattern]>, pub Option<Box<Expression>>, pub Meta); // optional exp is a guard condition
