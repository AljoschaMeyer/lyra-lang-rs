use im::{Vector, OrdSet, OrdMap};
use num::{BigRational, BigInt, ToPrimitive};

use super::{CmpF64, CmpRope};
use super::syntax::*;

/// Runtime representation of an arbitrary lyra value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Nil,
    Bool(bool),
    Usize(usize),
    Isize(isize),
    Int(BigInt),
    Ratio(BigRational),
    Float(CmpF64),
    Char(char),
    String(CmpRope),
    Sequence(Vector<Object>),
    Set(OrdSet<Object>),
    Map(OrdMap<Object, Object>),
    Fun, // TODO
}

impl From<()> for Value {
    fn from(_exp: ()) -> Value {
        Value::Nil
    }
}

impl From<bool> for Value {
    fn from(exp: bool) -> Value {
        Value::Bool(exp)
    }
}

impl From<&BigRational> for Value {
    fn from(exp: &BigRational) -> Value {
        if exp.is_integer() {
            let int = exp.to_integer();
            if let Some(n) = int.to_usize() {
                Value::Usize(n)
            } else if let Some(n) = int.to_isize() {
                Value::Isize(n)
            } else {
                Value::Int(int)
            }
        } else {
            Value::Ratio(exp.clone())
        }
    }
}

impl From<CmpF64> for Value {
    fn from(exp: CmpF64) -> Value {
        Value::Float(exp)
    }
}

impl From<char> for Value {
    fn from(exp: char) -> Value {
        Value::Char(exp)
    }
}

impl From<&str> for Value {
    fn from(exp: &str) -> Value {
        Value::String(CmpRope::from_str(exp))
    }
}

impl From<Vector<Object>> for Value {
    fn from(exp: Vector<Object>) -> Value {
        Value::Sequence(exp)
    }
}

impl From<OrdSet<Object>> for Value {
    fn from(exp: OrdSet<Object>) -> Value {
        Value::Set(exp)
    }
}

impl From<OrdMap<Object, Object>> for Value {
    fn from(exp: OrdMap<Object, Object>) -> Value {
        Value::Map(exp)
    }
}

/// An object is a value with an associated prototype.
/// TODO make the prototype an actual thing rather than Unit
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Object(pub Value, pub ());
