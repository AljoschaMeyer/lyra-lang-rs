use im::OrdMap;

use super::value::*;
use super::syntax::{Id, Meta};

lazy_static! {
    static ref FREE_VAR: OrdMap<Object, Object> = {
        let mut map = OrdMap::new();
        map.insert(Object("tag".into(), ()), Object("free_identifier".into(), ()));
        map.insert(Object("description".into(), ()), Object("Tried to evaluate an identifier that has not been bound to a value.".into(), ()));
        map
    };

    static ref NON_FUN_CALL: OrdMap<Object, Object> = {
        let mut map = OrdMap::new();
        map.insert(Object("tag".into(), ()), Object("non_fun_call".into(), ()));
        map.insert(Object("description".into(), ()), Object("Tried to invoke a non-function value".into(), ()));
        map
    };
}

pub fn free_identifier(id: &Id, meta: &Meta) -> Object {
    let mut map = FREE_VAR.clone();
    map.insert(Object("identifier".into(), ()), Object(id.0.as_str().into(), ()));
    map.insert(Object("meta".into(), ()), Object::from_meta(meta));

    Object(map.into(), ())
}

pub fn non_fun_call(val: &Value, meta: &Meta) -> Object {
    let mut map = NON_FUN_CALL.clone();
    map.insert(Object("val".into(), ()), Object(val.clone(), ()));
    map.insert(Object("meta".into(), ()), Object::from_meta(meta));

    Object(map.into(), ())
}
