use im::Vector;

use super::super::value::*;

pub fn land(args: Vector<Object>) -> Result<Object, Object> {
    for obj in args.iter() {
        if !obj.0.truthyness() {
            return Ok(Object(Value::Bool(false), ()));
        }
    }

    return Ok(Object(Value::Bool(true), ()));
}

pub fn lor(args: Vector<Object>) -> Result<Object, Object> {
    for obj in args.iter() {
        if obj.0.truthyness() {
            return Ok(Object(Value::Bool(true), ()));
        }
    }

    return Ok(Object(Value::Bool(false), ()));
}
