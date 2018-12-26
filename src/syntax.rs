use std::path::PathBuf;
use std::rc::Rc;

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
    Land(Box<Expression>, Box<Expression>),
    Lor(Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<[Statement]>, Option<Box<[Statement]>>),
    While(Box<Expression>, Box<[Statement]>),
    Try(Box<[Statement]>, Pattern, Box<[Statement]>),
    Case(Box<Expression>, Box<[(Patterns, Box<[Statement]>)]>),
    Loop(Box<Expression>, Box<[(Patterns, Box<[Statement]>)]>),
    Application(Box<Expression>, Box<[Expression]>),
    Fun(Box<[Pattern]>, Rc<Box<[Statement]>>),
    
    // operators, literals, for, map access, indexing
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Statement(pub _Statement, pub Meta);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum _Statement {
    Exp(Expression),
    Let(Pattern, Expression),
    Assign(String, Expression),
    Throw(Expression),
    Return(Expression),
    Break(Expression),
    
    //  rec, let await?, await for?
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
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Patterns(pub Box<[Pattern]>, pub Option<Box<Expression>>, pub Meta); // optional exp is a guard condition