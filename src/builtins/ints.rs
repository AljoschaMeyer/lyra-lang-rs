use ref_thread_local::RefThreadLocal;
use rug::{Integer, ops::Pow};

use super::super::semantics::{Value, _Reason};
use super::{ERR_TYPE, ERR_ZERO, ERR_ROOT, ERR_NEGATIVE, ERR_U32};

macro_rules! as_int {
    ($val:expr, $name:expr, $index:expr) => (
        match $val {
            Value::Int(ref int) => int,
            _ => return Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
                expected: "integer",
                got: $val.clone(),
                name: $name,
                index: $index,
            })),
        }
    )
}

macro_rules! as_int_u32 {
    ($val:expr, $name:expr, $index:expr) => (
        match $val {
            Value::Int(ref int) => {
                match int.to_u32() {
                    Some(int) => int,
                    None => {
                        return Err((ERR_U32.borrow().clone(), _Reason::U32Error {
                            got: $val.clone(),
                            name: $name,
                            index: $index,
                        }));
                    }
                }
            },
            _ => return Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
                expected: "integer",
                got: $val.clone(),
                name: $name,
                index: $index,
            })),
        }
    )
}

macro_rules! as_int_non_zero {
    ($val:expr, $name:expr, $index:expr) => (
        match $val {
            Value::Int(ref int) => {
                if *int == 0 {
                    return Err((ERR_ZERO.borrow().clone(), _Reason::ZeroError {
                        got: $val.clone(),
                        name: $name,
                        index: $index,
                    }));
                } else {
                    int
                }
            },
            _ => return Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
                expected: "integer",
                got: $val.clone(),
                name: $name,
                index: $index,
            })),
        }
    )
}

macro_rules! as_int_u32_non_zero {
    ($val:expr, $name:expr, $index:expr) => (
        match $val {
            Value::Int(ref int) => {
                match int.to_u32() {
                    Some(int) => {
                        if int == 0 {
                            return Err((ERR_ZERO.borrow().clone(), _Reason::ZeroError {
                                got: $val.clone(),
                                name: $name,
                                index: $index,
                            }));
                        } else {
                            int
                        }
                    }
                    None => {
                        return Err((ERR_U32.borrow().clone(), _Reason::U32Error {
                            got: $val.clone(),
                            name: $name,
                            index: $index,
                        }));
                    }
                }
            },
            _ => return Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
                expected: "integer",
                got: $val.clone(),
                name: $name,
                index: $index,
            })),
        }
    )
}

macro_rules! as_int_non_negative {
    ($val:expr, $name:expr, $index:expr) => (
        match $val {
            Value::Int(ref int) => {
                if *int < 0 {
                    return Err((ERR_NEGATIVE.borrow().clone(), _Reason::NegativeError {
                        got: $val.clone(),
                        name: $name,
                        index: $index,
                    }));
                } else {
                    int
                }
            },
            _ => return Err((ERR_TYPE.borrow().clone(), _Reason::TypeError {
                expected: "integer",
                got: $val.clone(),
                name: $name,
                index: $index,
            })),
        }
    )
}

pub fn int_is_even(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int!(val, "int_is_even", 0);
    Ok(Value::Bool(int.is_even()))
}

pub fn int_is_odd(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int!(val, "int_is_odd", 0);
    Ok(Value::Bool(int.is_odd()))
}

// returns false if second arg is zero
pub fn int_is_divisible(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_is_divisible", 0);
    let b = as_int!(b, "int_is_divisible", 1);
    Ok(Value::Bool(a.is_divisible(b)))
}

pub fn int_is_power_of_two(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int!(val, "int_is_power_of_two", 0);
    Ok(Value::Bool(int.is_power_of_two()))
}

pub fn int_significant_bits(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int!(val, "int_significant_bits", 0);
    Ok(Value::Int(int.significant_bits().into()))
}

pub fn int_signed_bits(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int!(val, "int_signed_bits", 0);
    Ok(Value::Int(int.signed_bits().into()))
}

pub fn int_abs(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int!(val, "int_abs", 0);
    Ok(Value::Int(int.abs_ref().into()))
}

pub fn int_signum(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int!(val, "int_signum", 0);
    Ok(Value::Int(int.signum_ref().into()))
}

// Returns 1 if val <= 0
pub fn int_next_power_of_two(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int!(val, "int_next_power_of_two", 0);
    Ok(Value::Int(int.next_power_of_two_ref().into()))
}

pub fn int_mod(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_mod", 0);
    let b = as_int_non_zero!(b, "int_mod", 1);

    let (_, modulus) = <(Integer, Integer)>::from(a.div_rem_euc_ref(&b));
    Ok(Value::Int(modulus.into()))
}

pub fn int_pow(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_pow", 0);
    let b = as_int_u32!(b, "int_pow", 1);

    Ok(Value::Int(a.pow(b).into()))
}

pub fn int_root(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_root", 0);
    let b = as_int_u32_non_zero!(b, "int_root", 1);

    if a.cmp0() == std::cmp::Ordering::Less && (b % 2 == 0) {
        Err((ERR_ROOT.borrow().clone(), _Reason::RootError {
            a: a.clone(),
            b,
        }))
    } else {
        Ok(Value::Int(a.root_ref(b).into()))
    }
}

pub fn int_root_rem(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_root_rem", 0);
    let b = as_int_u32_non_zero!(b, "int_root_rem", 1);

    if a.cmp0() == std::cmp::Ordering::Less && (b % 2 == 0) {
        Err((ERR_ROOT.borrow().clone(), _Reason::RootError {
            a: a.clone(),
            b,
        }))
    } else {
        let (_, rem) = <(Integer, Integer)>::from(a.root_rem_ref(b));
        Ok(Value::Int(rem.into()))
    }
}

pub fn int_square(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int!(val, "int_square", 0);
    Ok(Value::Int(int.square_ref().into()))
}

pub fn int_sqrt(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int_non_negative!(val, "int_sqrt", 0);
    Ok(Value::Int(int.sqrt_ref().into()))
}

pub fn int_sqrt_rem(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int_non_negative!(val, "int_sqrt", 0);
    let (_, rem) = <(Integer, Integer)>::from(int.sqrt_rem_ref());
    Ok(Value::Int(rem.into()))
}

pub fn int_factorial(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int_u32!(val, "int_factorial", 0);

    Ok(Value::Int(Integer::factorial(int).into()))
}

pub fn int_negate(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int!(val, "int_negate", 0);
    Ok(Value::Int((-int).into()))
}

pub fn int_add(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_add", 0);
    let b = as_int!(b, "int_add", 1);
    Ok(Value::Int((a + b).into()))
}

pub fn int_sub(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_sub", 0);
    let b = as_int!(b, "int_sub", 1);
    Ok(Value::Int((a - b).into()))
}

pub fn int_mul(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_mul", 0);
    let b = as_int!(b, "int_mul", 1);
    Ok(Value::Int((a * b).into()))
}

pub fn int_div(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_div", 0);
    let b = as_int!(b, "int_div", 1);
    Ok(Value::Int((a / b).into()))
}

pub fn int_rem(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_rem", 0);
    let b = as_int!(b, "int_rem", 1);
    Ok(Value::Int((a % b).into()))
}

// two's complement (treats positive numbers as if they had a leading zero bit, then bitwise not)
pub fn int_not(val: Value) -> Result<Value, (Value, _Reason)> {
    let int = as_int!(val, "int_not", 0);
    Ok(Value::Int((!int).into()))
}

pub fn int_and(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_and", 0);
    let b = as_int!(b, "int_and", 1);
    Ok(Value::Int((a & b).into()))
}

pub fn int_or(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_or", 0);
    let b = as_int!(b, "int_or", 1);
    Ok(Value::Int((a | b).into()))
}

pub fn int_xor(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_xor", 0);
    let b = as_int!(b, "int_xor", 1);
    Ok(Value::Int((a ^ b).into()))
}

pub fn int_shift_left(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_shift_left", 0);
    let b = as_int_u32!(b, "int_shift_left", 1);
    Ok(Value::Int((a << b).into()))
}

pub fn int_shift_right(a: Value, b: Value) -> Result<Value, (Value, _Reason)> {
    let a = as_int!(a, "int_shift_right", 0);
    let b = as_int_u32!(b, "int_shift_right", 1);
    Ok(Value::Int((a >> b).into()))
}

// TODO as_bytes
