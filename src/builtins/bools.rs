use ref_thread_local::RefThreadLocal;

use super::super::semantics::{Value, _Reason};
use super::ERR_TYPE;

pub fn bool_not(val: Value) -> Result<Value, (Value, _Reason)> {
    match val {
        Value::Bool(b) => Ok(Value::Bool(!b)),
        _ => Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
            expected: "bool",
            got: val.clone(),
            name: "bool_not",
            index: 0,
        })),
    }
}

pub fn bool_and(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    match (a.clone(), b.clone()) {
        (Value::Bool(b0), Value::Bool(b1)) => Ok(Value::Bool(b0 && b1)),
        (Value::Bool(..), _) => Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
            expected: "bool",
            got: b.clone(),
            name: "bool_and",
            index: 1,
        })),
        _ => Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
            expected: "bool",
            got: a.clone(),
            name: "bool_and",
            index: 0,
        })),
    }
}

pub fn bool_or(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    match (a.clone(), b.clone()) {
        (Value::Bool(b0), Value::Bool(b1)) => Ok(Value::Bool(b0 || b1)),
        (Value::Bool(..), _) => Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
            expected: "bool",
            got: b.clone(),
            name: "bool_or",
            index: 1,
        })),
        _ => Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
            expected: "bool",
            got: a.clone(),
            name: "bool_or",
            index: 0,
        })),
    }
}

pub fn bool_imply(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    match (a.clone(), b.clone()) {
        (Value::Bool(b0), Value::Bool(b1)) => Ok(Value::Bool(!b1 || b0)),
        (Value::Bool(..), _) => Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
            expected: "bool",
            got: b.clone(),
            name: "bool_imply",
            index: 1,
        })),
        _ => Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
            expected: "bool",
            got: a.clone(),
            name: "bool_imply",
            index: 0,
        })),
    }
}

pub fn bool_eq(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    match (a.clone(), b.clone()) {
        (Value::Bool(b0), Value::Bool(b1)) => Ok(Value::Bool(b0 == b1)),
        (Value::Bool(..), _) => Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
            expected: "bool",
            got: b.clone(),
            name: "bool_eq",
            index: 1,
        })),
        _ => Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
            expected: "bool",
            got: a.clone(),
            name: "bool_eq",
            index: 0,
        })),
    }
}

pub fn bool_xor(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    match (a.clone(), b.clone()) {
        (Value::Bool(b0), Value::Bool(b1)) => Ok(Value::Bool(b0 != b1)),
        (Value::Bool(..), _) => Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
            expected: "bool",
            got: b.clone(),
            name: "bool_xor",
            index: 1,
        })),
        _ => Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
            expected: "bool",
            got: a.clone(),
            name: "bool_xor",
            index: 0,
        })),
    }
}