use std::sync::Arc;

use im::{HashMap, Vector};

use super::CmpRope;
use super::value::*;
use super::syntax::Id;

mod bools;
use self::bools::*;

lazy_static! {
    static ref TOP_LEVEL: HashMap<Id, Value> = {
        let mut map = HashMap::new();
        map.insert(Id("stringify".to_string()), Value::Fun(_Fun::Rust(stringify)));
        map.insert(Id("typeof".to_string()), Value::Fun(_Fun::Rust(type_of)));
        map.insert(Id("land".to_string()), Value::Fun(_Fun::Rust(land)));
        map.insert(Id("lor".to_string()), Value::Fun(_Fun::Rust(lor)));
        map
    };
}

fn stringify(args: Vector<Value>) -> Result<Value, Value> {
    match args.front() {
        Some(val) => Ok(Value::String(Arc::new(CmpRope(val.to_rope())))),
        None => Ok("nil".into()),
    }
}

fn type_of(args: Vector<Value>) -> Result<Value, Value> {
    match args.front() {
        Some(Value::Nil) => Ok("nil".into()),
        Some(Value::Bool(..)) => Ok("bool".into()),
        Some(Value::Usize(..))
        | Some(Value::Isize(..))
        | Some(Value::Int(..))
        | Some(Value::Ratio(..)) => Ok("number".into()),
        Some(Value::Char(..)) => Ok("char".into()),
        Some(Value::String(..)) => Ok("string".into()),
        Some(Value::Sequence(..)) => Ok("sequence".into()),
        Some(Value::Set(..)) => Ok("set".into()),
        Some(Value::Map(..)) => Ok("map".into()),
        Some(Value::Fun(..)) => Ok("function".into()),
        None => Ok("nil".into()),
    }
}

pub fn top_level() -> HashMap<Id, Value> {
    TOP_LEVEL.clone()
}
