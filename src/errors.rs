use im::OrdMap;

use super::value::*;
use super::syntax::{Id, Meta};

lazy_static! {
    static ref FREE_VAR: OrdMap<Value, Value> = {
        let mut map = OrdMap::new();
        map.insert("tag".into(), "free_identifier".into());
        map.insert("description".into(), "Tried to evaluate an identifier that has not been bound to a value.".into());
        map
    };

    static ref NON_FUN_CALL: OrdMap<Value, Value> = {
        let mut map = OrdMap::new();
        map.insert("tag".into(), "non_fun_call".into());
        map.insert("description".into(), "Tried to invoke a non-function value".into());
        map
    };
}

pub fn free_identifier(id: &Id, meta: &Meta) -> Value {
    let mut map = FREE_VAR.clone();
    map.insert("identifier".into(), id.0.as_str().into());
    map.insert("meta".into(), Value::from_meta(meta));

    map.into()
}

pub fn non_fun_call(val: &Value, meta: &Meta) -> Value {
    let mut map = NON_FUN_CALL.clone();
    map.insert("val".into(), val.clone());
    map.insert("meta".into(), Value::from_meta(meta));

    map.into()
}
