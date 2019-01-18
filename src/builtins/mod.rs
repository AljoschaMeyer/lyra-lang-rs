use ref_thread_local::RefThreadLocal;

use super::semantics::{Value, Environment, _Fun, _Reason, truthy};

mod bools;
use self::bools::*;
mod ints;
use self::ints::*;

ref_thread_local! {
    pub static managed ERR_NOT_A_FUNCTION: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_TYPE: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_ZERO: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_ROOT: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_NEGATIVE: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_U32: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_REFUTED_NIL: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_REFUTED_BOOL: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_REFUTED_INT: Value = Value::Nil; // TODO turn into string
    pub static managed ERR_REFUTED_FLOAT: Value = Value::Nil; // TODO turn into string


    pub static managed TOPLEVEL: Environment = {
        let mut env = Environment::empty();
        env = env.insert("err_not_a_function".to_string(), ERR_NOT_A_FUNCTION.borrow().clone(), false);
        env = env.insert("err_type".to_string(), ERR_TYPE.borrow().clone(), false);
        env = env.insert("err_zero".to_string(), ERR_ZERO.borrow().clone(), false);
        env = env.insert("err_root".to_string(), ERR_ROOT.borrow().clone(), false);
        env = env.insert("err_negative".to_string(), ERR_NEGATIVE.borrow().clone(), false);
        env = env.insert("err_u32".to_string(), ERR_U32.borrow().clone(), false);
        env = env.insert("err_refuted_nil".to_string(), ERR_REFUTED_NIL.borrow().clone(), false);
        env = env.insert("err_refuted_bool".to_string(), ERR_REFUTED_BOOL.borrow().clone(), false);
        env = env.insert("err_refuted_int".to_string(), ERR_REFUTED_INT.borrow().clone(), false);
        env = env.insert("err_refuted_float".to_string(), ERR_REFUTED_FLOAT.borrow().clone(), false);

        env = env.insert("halt".to_string(), Value::Fun(_Fun::Native0(halt)), false);
        env = env.insert("is_truthy".to_string(), Value::Fun(_Fun::Native1(is_truthy)), false);
        env = env.insert("is_not_truthy".to_string(), Value::Fun(_Fun::Native1(is_not_truthy)), false);
        env = env.insert("equal".to_string(), Value::Fun(_Fun::Native2(eq)), false);
        env = env.insert("not_equal".to_string(), Value::Fun(_Fun::Native2(neq)), false);
        env = env.insert("lesser_than".to_string(), Value::Fun(_Fun::Native2(lt)), false);
        env = env.insert("lesser_equal_than".to_string(), Value::Fun(_Fun::Native2(lte)), false);
        env = env.insert("greater_than".to_string(), Value::Fun(_Fun::Native2(gt)), false);
        env = env.insert("greater_equal_than".to_string(), Value::Fun(_Fun::Native2(gte)), false);

        env = env.insert("is_nil".to_string(), Value::Fun(_Fun::Native1(is_nil)), false);
        env = env.insert("is_bool".to_string(), Value::Fun(_Fun::Native1(is_bool)), false);
        env = env.insert("is_int".to_string(), Value::Fun(_Fun::Native1(is_int)), false);
        env = env.insert("is_nat".to_string(), Value::Fun(_Fun::Native1(is_nat)), false);

        env = env.insert("bool_not".to_string(), Value::Fun(_Fun::Native1(bool_not)), false);
        env = env.insert("bool_and".to_string(), Value::Fun(_Fun::Native2(bool_and)), false);
        env = env.insert("bool_or".to_string(), Value::Fun(_Fun::Native2(bool_or)), false);
        env = env.insert("bool_imply".to_string(), Value::Fun(_Fun::Native2(bool_imply)), false);
        env = env.insert("bool_equivalent".to_string(), Value::Fun(_Fun::Native2(bool_equivalent)), false);
        env = env.insert("bool_xor".to_string(), Value::Fun(_Fun::Native2(bool_xor)), false);

        env = env.insert("int_is_even".to_string(), Value::Fun(_Fun::Native1(int_is_even)), false);
        env = env.insert("int_is_odd".to_string(), Value::Fun(_Fun::Native1(int_is_odd)), false);
        env = env.insert("int_is_divisible".to_string(), Value::Fun(_Fun::Native2(int_is_divisible)), false);
        env = env.insert("int_is_power_of_two".to_string(), Value::Fun(_Fun::Native1(int_is_power_of_two)), false);
        env = env.insert("int_significant_bits".to_string(), Value::Fun(_Fun::Native1(int_significant_bits)), false);
        env = env.insert("int_signed_bits".to_string(), Value::Fun(_Fun::Native1(int_signed_bits)), false);
        env = env.insert("int_abs".to_string(), Value::Fun(_Fun::Native1(int_abs)), false);
        env = env.insert("int_signum".to_string(), Value::Fun(_Fun::Native1(int_signum)), false);
        env = env.insert("int_next_power_of_two".to_string(), Value::Fun(_Fun::Native1(int_next_power_of_two)), false);
        env = env.insert("int_mod".to_string(), Value::Fun(_Fun::Native2(int_mod)), false);
        env = env.insert("int_pow".to_string(), Value::Fun(_Fun::Native2(int_pow)), false);
        env = env.insert("int_root".to_string(), Value::Fun(_Fun::Native2(int_root)), false);
        env = env.insert("int_root_rem".to_string(), Value::Fun(_Fun::Native2(int_root_rem)), false);
        env = env.insert("int_square".to_string(), Value::Fun(_Fun::Native1(int_square)), false);
        env = env.insert("int_sqrt".to_string(), Value::Fun(_Fun::Native1(int_sqrt)), false);
        env = env.insert("int_sqrt_rem".to_string(), Value::Fun(_Fun::Native1(int_sqrt_rem)), false);
        env = env.insert("int_factorial".to_string(), Value::Fun(_Fun::Native1(int_factorial)), false);
        env = env.insert("int_negate".to_string(), Value::Fun(_Fun::Native1(int_negate)), false);
        env = env.insert("int_add".to_string(), Value::Fun(_Fun::Native2(int_add)), false);
        env = env.insert("int_sub".to_string(), Value::Fun(_Fun::Native2(int_sub)), false);
        env = env.insert("int_mul".to_string(), Value::Fun(_Fun::Native2(int_mul)), false);
        env = env.insert("int_div".to_string(), Value::Fun(_Fun::Native2(int_div)), false);
        env = env.insert("int_rem".to_string(), Value::Fun(_Fun::Native2(int_rem)), false);
        env = env.insert("int_not".to_string(), Value::Fun(_Fun::Native1(int_not)), false);
        env = env.insert("int_and".to_string(), Value::Fun(_Fun::Native2(int_and)), false);
        env = env.insert("int_or".to_string(), Value::Fun(_Fun::Native2(int_or)), false);
        env = env.insert("int_xor".to_string(), Value::Fun(_Fun::Native2(int_xor)), false);
        env = env.insert("int_shift_left".to_string(), Value::Fun(_Fun::Native2(int_shift_left)), false);
        env = env.insert("int_shift_right".to_string(), Value::Fun(_Fun::Native2(int_shift_right)), false);

        // TODO fn_apply ?
        env
    };
}

fn halt() -> Result<Value, (Value, _Reason)> {
    panic!("called the built-in `halt` function")
}

fn is_truthy(val: Value) -> Result<Value, (Value, _Reason)> {
    Ok(Value::Bool(truthy(&val)))
}

pub fn is_not_truthy(val: Value) -> Result<Value, (Value, _Reason)> {
    Ok(Value::Bool(!truthy(&val)))
}

pub fn eq(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    match (a, b) {
        (Value::Float(a), Value::Float(b)) if a.0.is_nan() || b.0.is_nan() => Ok(Value::Bool(false)),
        (a, b) => Ok(Value::Bool(a == b)),
    }
}

pub fn neq(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    match (a, b) {
        (Value::Float(a), Value::Float(b)) if a.0.is_nan() || b.0.is_nan() => Ok(Value::Bool(true)),
        (a, b) => Ok(Value::Bool(a != b)),
    }
}

pub fn lt(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    match (a, b) {
        (Value::Float(a), Value::Float(b)) if a.0.is_nan() || b.0.is_nan() => Ok(Value::Bool(false)),
        (a, b) => Ok(Value::Bool(a < b)),
    }
}

pub fn lte(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    match (a, b) {
        (Value::Float(a), Value::Float(b)) if a.0.is_nan() || b.0.is_nan() => Ok(Value::Bool(false)),
        (a, b) => Ok(Value::Bool(a <= b)),
    }
}

pub fn gt(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    match (a, b) {
        (Value::Float(a), Value::Float(b)) if a.0.is_nan() || b.0.is_nan() => Ok(Value::Bool(false)),
        (a, b) => Ok(Value::Bool(a > b)),
    }
}

pub fn gte(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    match (a, b) {
        (Value::Float(a), Value::Float(b)) if a.0.is_nan() || b.0.is_nan() => Ok(Value::Bool(false)),
        (a, b) => Ok(Value::Bool(a >= b)),
    }
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

fn is_int(val: Value) -> Result<Value, (Value, _Reason)> {
    match val {
        Value::Int(..) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn is_nat(val: Value) -> Result<Value, (Value, _Reason)> {
    match val {
        Value::Int(ref int) => Ok(Value::Bool(int.cmp0() != std::cmp::Ordering::Less)),
        _ => Ok(Value::Bool(false)),
    }
}
