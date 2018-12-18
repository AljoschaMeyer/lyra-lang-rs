#![feature(try_from)]

extern crate failure;
#[macro_use] extern crate failure_derive;
extern crate im_rc;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate gc_derive;
#[macro_use] extern crate gc;
extern crate num;
extern crate ropey;

pub mod syntax;
pub mod parser;
pub mod semantics;

#[cfg(test)]
mod tests {
    use super::parser::Parser;
    use super::syntax::Source;
    use super::semantics::{Value, exec_many, initial_env};

    fn run(src: &str) -> Result<Value, Value> {
        let mut p = Parser::new(src, Source::other());
        let program = p.p_program().unwrap();
        let eval = exec_many(&mut program.iter(), initial_env());
        assert!(p.end());
        eval.1
    }

    #[test]
    fn test_nil() {
        assert_eq!(run("nil; nil").unwrap(), Value::Nil);
    }

    #[test]
    fn test_bool() {
        assert_eq!(run(" true").unwrap(), Value::Bool(true));
        assert_eq!(run("false ").unwrap(), Value::Bool(false));
    }
}
