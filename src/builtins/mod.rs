use ref_thread_local::RefThreadLocal;

use super::semantics::{Value, Environment, _Fun, _Reason};

ref_thread_local! {
    pub static managed ERR_NOT_A_FUNCTION: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_REFUTED_NIL: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_REFUTED_BOOL: Value = Value::Nil; // TODO turn into string
    
    pub static managed HALT: Value = Value::Fun(_Fun::Native0(halt));
    
    pub static managed TOPLEVEL: Environment = {
        let mut env = Environment::empty();
        env = env.insert("err_not_a_function".to_string(), ERR_NOT_A_FUNCTION.borrow().clone(), false);
        env = env.insert("err_refuted_nil".to_string(), ERR_REFUTED_NIL.borrow().clone(), false);
        env = env.insert("err_refuted_bool".to_string(), ERR_REFUTED_BOOL.borrow().clone(), false);
        
        env = env.insert("halt".to_string(), HALT.borrow().clone(), false);
        
        env
    };
}

fn halt() -> Result<Value, (Value, _Reason)> {
    panic!("called the halt built-in function")
}