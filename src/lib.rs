#![feature(try_from)]

extern crate either;
extern crate failure;
extern crate im;
#[macro_use] extern crate failure_derive;
extern crate num;
extern crate ropey;
extern crate strtod;

pub mod value;
pub mod parser;
pub mod syntax;
pub mod evaluate;

use std::cmp::Ordering;
use ropey::Rope;

/// A wrapper around f64 that lets two NaNs compare to equal and orders them lower than all other floats.
#[derive(Clone, Copy, Debug, PartialOrd)]
pub struct CmpF64(pub f64);

impl PartialEq for CmpF64 {
    fn eq(&self, other: &CmpF64) -> bool {
        if self.0.is_nan() && other.0.is_nan() {
            true
        } else {
            self.0 == other.0
        }
    }
}
impl Eq for CmpF64 {}

impl Ord for CmpF64 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or_else(|| {
            if self.0.is_nan() && !other.0.is_nan() {
                Ordering::Less
            } else if !self.0.is_nan() && other.0.is_nan() {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        })
    }
}

/// A wrapper around Rope that naively implements Eq, PartialOrd and Ord in O(n), by
/// iterating over all bytes and comparing them.
///
/// See https://github.com/cessen/ropey/issues/17
#[derive(Clone, Debug)]
pub struct CmpRope(pub Rope);

impl CmpRope {
    pub fn from_str(text: &str) -> Self {
        CmpRope(Rope::from_str(text))
    }
}

impl PartialEq for CmpRope {
    fn eq(&self, other: &Self) -> bool {
        let mut other_bytes = other.0.bytes();
        for byte in self.0.bytes() {
            match other_bytes.next() {
                None => return false,
                Some(other_byte) => {
                    if byte != other_byte {
                        return false
                    }
                }
            }
        }

        match other_bytes.next() {
            None => return true,
            Some(_) => return false,
        }
    }
}
impl Eq for CmpRope {}

impl PartialOrd for CmpRope {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CmpRope {
    fn cmp(&self, other: &Self) -> Ordering {
        let mut other_bytes = other.0.bytes();
        for byte in self.0.bytes() {
            match other_bytes.next() {
                None => return Ordering::Greater,
                Some(other_byte) => {
                    match byte.cmp(&other_byte) {
                        Ordering::Greater => return Ordering::Greater,
                        Ordering::Less => return Ordering::Less,
                        Ordering::Equal => {}
                    }
                }
            }
        }

        match other_bytes.next() {
            None => return Ordering::Less,
            Some(_) => return Ordering::Equal,
        }
    }
}
