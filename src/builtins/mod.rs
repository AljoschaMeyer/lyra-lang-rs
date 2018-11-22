use std::sync::Arc;

use im::{HashMap, Vector};

use super::CmpRope;
use super::value::*;
use super::syntax::Id;

mod bools;
use self::bools::*;

lazy_static! {
    static ref TOP_LEVEL: HashMap<Id, Object> = {
        let mut map = HashMap::new();
        map.insert(Id("stringify".to_string()), Object(Value::Fun(_Fun::Rust(stringify)), ()));
        map.insert(Id("typeof".to_string()), Object(Value::Fun(_Fun::Rust(type_of)), ()));
        map.insert(Id("land".to_string()), Object(Value::Fun(_Fun::Rust(land)), ()));
        map.insert(Id("lor".to_string()), Object(Value::Fun(_Fun::Rust(lor)), ()));
        map
    };
}

fn stringify(args: Vector<Object>) -> Result<Object, Object> {
    match args.front() {
        Some(obj) => Ok(Object(Value::String(Arc::new(CmpRope(obj.0.to_rope()))), ())),
        None => Ok(Object("nil".into(), ())),
    }
}

fn type_of(args: Vector<Object>) -> Result<Object, Object> {
    match args.front() {
        Some(Object(Value::Nil, _)) => Ok(Object("nil".into(), ())),
        Some(Object(Value::Bool(..), _)) => Ok(Object("bool".into(), ())),
        Some(Object(Value::Usize(..), _))
        | Some(Object(Value::Isize(..), _))
        | Some(Object(Value::Int(..), _))
        | Some(Object(Value::Ratio(..), _)) => Ok(Object("number".into(), ())),
        Some(Object(Value::Float(..), _)) => Ok(Object("float".into(), ())),
        Some(Object(Value::Char(..), _)) => Ok(Object("char".into(), ())),
        Some(Object(Value::String(..), _)) => Ok(Object("string".into(), ())),
        Some(Object(Value::Sequence(..), _)) => Ok(Object("sequence".into(), ())),
        Some(Object(Value::Set(..), _)) => Ok(Object("set".into(), ())),
        Some(Object(Value::Map(..), _)) => Ok(Object("map".into(), ())),
        Some(Object(Value::Fun(..), _)) => Ok(Object("function".into(), ())),
        None => Ok(Object("nil".into(), ())),
    }
}

pub fn top_level() -> HashMap<Id, Object> {
    TOP_LEVEL.clone()
}
