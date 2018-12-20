use super::semantics::Value;

lazy_static! {
    pub static ref ERR_REFUTED_NIL: Value = Value::Nil; // TODO turn into string
    pub static ref ERR_REFUTED_BOOL: Value = Value::Nil; // TODO turn into string
}