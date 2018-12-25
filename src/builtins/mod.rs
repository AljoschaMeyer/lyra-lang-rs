use ref_thread_local::RefThreadLocal;

use super::semantics::{Value, Environment, _Fun, _Reason, truthy};

mod bools;
use self::bools::*;

ref_thread_local! {
    pub static managed ERR_NOT_A_FUNCTION: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_TYPE: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_REFUTED_NIL: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_REFUTED_BOOL: Value = Value::Nil; // TODO turn into string
    
    pub static managed HALT: Value = Value::Fun(_Fun::Native0(halt));
    pub static managed IS_TRUTHY: Value = Value::Fun(_Fun::Native1(is_truthy));
    // TODO eq and comparisions (also syntax sugar for those)
    
    pub static managed IS_NIL: Value = Value::Fun(_Fun::Native1(is_nil));
    pub static managed IS_BOOL: Value = Value::Fun(_Fun::Native1(is_bool));
    
    pub static managed BOOL_NOT: Value = Value::Fun(_Fun::Native1(bool_not));
    pub static managed BOOL_AND: Value = Value::Fun(_Fun::Native2(bool_and));
    pub static managed BOOL_OR: Value = Value::Fun(_Fun::Native2(bool_or));
    pub static managed BOOL_IMPLY: Value = Value::Fun(_Fun::Native2(bool_imply));
    pub static managed BOOL_EQ: Value = Value::Fun(_Fun::Native2(bool_eq));
    pub static managed BOOL_XOR: Value = Value::Fun(_Fun::Native2(bool_xor));
    
    pub static managed TOPLEVEL: Environment = {
        let mut env = Environment::empty();
        env = env.insert("err_not_a_function".to_string(), ERR_NOT_A_FUNCTION.borrow().clone(), false);
        env = env.insert("err_type".to_string(), ERR_TYPE.borrow().clone(), false);
        env = env.insert("err_refuted_nil".to_string(), ERR_REFUTED_NIL.borrow().clone(), false);
        env = env.insert("err_refuted_bool".to_string(), ERR_REFUTED_BOOL.borrow().clone(), false);
        
        env = env.insert("halt".to_string(), HALT.borrow().clone(), false);
        env = env.insert("is_truthy".to_string(), IS_TRUTHY.borrow().clone(), false);
        
        env = env.insert("is_nil".to_string(), IS_NIL.borrow().clone(), false);
        env = env.insert("is_bool".to_string(), IS_BOOL.borrow().clone(), false);
        
        env = env.insert("bool_not".to_string(), BOOL_NOT.borrow().clone(), false);
        env = env.insert("bool_and".to_string(), BOOL_AND.borrow().clone(), false);
        env = env.insert("bool_or".to_string(), BOOL_OR.borrow().clone(), false);
        env = env.insert("bool_imply".to_string(), BOOL_IMPLY.borrow().clone(), false);
        env = env.insert("bool_eq".to_string(), BOOL_EQ.borrow().clone(), false);
        env = env.insert("bool_xor".to_string(), BOOL_XOR.borrow().clone(), false);
        
        env
    };
}

fn halt() -> Result<Value, (Value, _Reason)> {
    panic!("called the built-in `halt` function")
}

fn is_nil(val: Value) -> Result<Value, (Value, _Reason)> {
    match val {
        Value::Nil => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn is_bool(val: Value) -> Result<Value, (Value, _Reason)> {
    match val {
        Value::Bool(..) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn is_truthy(val: Value) -> Result<Value, (Value, _Reason)> {
    Ok(Value::Bool(truthy(&val)))
}