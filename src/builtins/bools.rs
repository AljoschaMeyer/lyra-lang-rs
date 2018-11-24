use im::Vector;

use super::super::value::*;

pub fn and(args: Vector<Value>) -> Result<Value, Value> {
    for val in args.iter() {
        if !val.truthyness() {
            return Ok(Value::Bool(false));
        }
    }

    return Ok(Value::Bool(true));
}

pub fn or(args: Vector<Value>) -> Result<Value, Value> {
    for val in args.iter() {
        if val.truthyness() {
            return Ok(Value::Bool(true));
        }
    }

    return Ok(Value::Bool(false));
}
