use std::collections::HashMap;

use im::{Vector, OrdSet, OrdMap};

use super::syntax::*;
use super::value::*;

/// All evaluation happens in an environment that provides the names for free variables.
#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    bindings: HashMap<Id, Object>,
    parent: Option<Box<Env>>,
}

/// The result of a (potentially partial) evaluation attempt.
pub enum Evaluation {
    /// The expression evaluated to this object.
    Yay(Object),
    /// The expression threw this object.
    Threw(Object),
    /// TODO Partial evaluation (for debugging)
    Paused
}
use self::Evaluation::*;

/// Short-circuiting evaluation similar to the std `try!` macro. Evaluate to the object in
/// a `Yay`, and directly returns on `Threw` or `Paused`.
macro_rules! try_eval {
    ($expr:expr) => (match $expr {
        Evaluation::Yay(obj) => obj,
        Evaluation::Threw(obj) => return Evaluation::Threw(obj),
        Evaluation::Paused => return Evaluation::Paused,
    });
    ($expr:expr,) => (try!($expr));
}

/// Evaluate an expression inside an environment. Mutates the environment by adding bindings
/// for each let expression.
pub fn evaluate(exp: &Expression, env: &mut Env) -> Evaluation {
    match exp.0 {
        _Expression::Nil => Yay(Object(().into(), ())),
        _Expression::Bool(inner) => Yay(Object(inner.into(), ())),
        _Expression::Int(ref inner) => Yay(Object(inner.into(), ())),
        _Expression::Float(inner) => Yay(Object(inner.into(), ())),
        _Expression::Char(inner) => Yay(Object(inner.into(), ())),
        _Expression::String(ref inner) => Yay(Object(inner.as_str().into(), ())),
        _Expression::Sequence(ref inners) => {
            let mut seq = Vector::new();
            for inner in inners.iter() {
                seq.push_back(try_eval!(evaluate(inner, env)));
            }
            Yay(Object(seq.into(), ()))
        }
        _Expression::Set(ref inners) => {
            let mut set = OrdSet::new();
            for inner in inners.0.iter() {
                set.insert(try_eval!(evaluate(inner, env)));
            }
            Yay(Object(set.into(), ()))
        }
        _Expression::Map(ref inners) => {
            let mut map = OrdMap::new();
            for inner in inners.iter() {
                map.insert(try_eval!(evaluate(&inner.0, env)), try_eval!(evaluate(&inner.1, env)));
            }
            Yay(Object(map.into(), ()))
        }




        _ => unimplemented!()
    }
}
