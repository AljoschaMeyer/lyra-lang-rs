#![feature(try_from)]

extern crate failure;
#[macro_use] extern crate failure_derive;
extern crate im;
#[macro_use] extern crate lazy_static;
extern crate num;
extern crate ropey;

pub mod value;
pub mod parser;
pub mod syntax;
pub mod evaluate;
pub mod errors;
pub mod builtins;

use std::cmp::Ordering;
use ropey::Rope;

/// A wrapper around Rope that naively implements Eq, PartialOrd and Ord in O(n), by
/// iterating over all bytes and comparing them.
///
/// See https://github.com/cessen/ropey/issues/17
#[derive(Clone, Debug, PartialEq)]
pub struct CmpRope(pub Rope);

impl CmpRope {
    pub fn from_str(text: &str) -> Self {
        CmpRope(Rope::from_str(text))
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use im::Vector;
    use ropey::Rope;

    use super::CmpRope;
    use super::parser::Parser;
    use super::value::*;
    use super::evaluate::{Env, evaluate, Evaluation::*};
    use super::syntax;

    fn parse(s: &str) -> syntax::Expression {
        let source = syntax::Source(Arc::new(syntax::_Source::Interactive));
        let mut parser = Parser::new(s, source);
        let exp = parser.p_exp().unwrap();
        assert!(parser.end());
        exp
    }

    #[test]
    fn lit_nil() {
        match evaluate(&parse("nil"), Env::root()) {
            Yay(Value::Nil) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn lit_args() {
        match evaluate(&parse("args"), Env::root()) {
            Yay(Value::Sequence(ref args)) => assert_eq!(args, &Vector::new()),
            _ => assert!(false),
        }
    }

    #[test]
    fn lit_true() {
        match evaluate(&parse("true"), Env::root()) {
            Yay(Value::Bool(true)) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn lit_false() {
        match evaluate(&parse("false"), Env::root()) {
            Yay(Value::Bool(false)) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn lit_small_int() {
        match evaluate(&parse("123"), Env::root()) {
            Yay(Value::Usize(123)) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn lit_char() {
        match evaluate(&parse("'💚'"), Env::root()) {
            Yay(Value::Char('💚')) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn lit_char_esc_quote() {
        match evaluate(&parse("'\\''"), Env::root()) {
            Yay(Value::Char('\'')) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn lit_char_esc_unicode() {
        match evaluate(&parse("'\u{1F49A}'"), Env::root()) {
            Yay(Value::Char('💚')) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn lit_string() {
        match evaluate(&parse(r#""a\u{123}\"'\n\t""#), Env::root()) {
            Yay(Value::String(rope)) => assert_eq!(rope.as_ref(), &CmpRope(Rope::from_str("a\u{123}\"'\n\t"))),
            _ => assert!(false),
        }
    }

    #[test]
    fn type_of() {
        match evaluate(&parse("typeof({4: @{}, [ 6 ]: @{true, false, 42}})"), Env::root()) {
            Yay(Value::String(rope)) => assert_eq!(rope.as_ref(), &CmpRope(Rope::from_str("map"))),
            Threw(nope) => {
                println!("{:#?}", nope);
                assert!(false)
            }
            _ => assert!(false)
        }
    }

    #[test]
    fn lets() {
        match evaluate(&parse("let a = (a) -> a && false; let b = () -> a(true); b() || 42"), Env::root()) {
            Yay(Value::Bool(true)) => {}
            Threw(nope) => {
                println!("{:?}", nope);
                assert!(false)
            }
            _ => assert!(false)
        }
    }
}
