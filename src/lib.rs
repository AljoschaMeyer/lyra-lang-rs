#![feature(try_from)]

extern crate failure;
#[macro_use] extern crate failure_derive;
extern crate im_rc;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate gc_derive;
#[macro_use] extern crate gc;
extern crate num;
extern crate ropey;

pub mod gc_foreign;
pub mod syntax;
pub mod parser;
pub mod semantics;
pub mod builtins;

#[cfg(test)]
mod tests {
    use super::parser::Parser;
    use super::syntax::Source;
    use super::semantics::{Value, exec_many, Environment, Reason};

    fn run(src: &str) -> Result<(Value, Environment), (Value, Reason)> {
        let mut p = Parser::new(src, Source::other());
        let program = p.p_program().unwrap();
        let eval = exec_many(&mut program.iter(), Environment::toplevel());
        assert!(p.end());
        eval
    }

    #[test]
    fn test_nil() {
        assert_eq!(run("nil; nil").unwrap().0, Value::Nil);
    }

    #[test]
    fn test_bool() {
        assert_eq!(run(" true").unwrap().0, Value::Bool(true));
        assert_eq!(run("false ").unwrap().0, Value::Bool(false));
    }
    
    #[test]
    fn test_let() {
        assert_eq!(run("let x = true").unwrap().0, Value::Nil);
        assert_eq!(run("let mut x = true").unwrap().0, Value::Nil);
        assert_eq!(run("let _ = true").unwrap().0, Value::Nil);
        assert_eq!(run("let _ = nil; true").unwrap().0, Value::Bool(true));
        assert_eq!(run("let x = true; x").unwrap().0, Value::Bool(true));
        assert_eq!(run("let mut x = true; x").unwrap().0, Value::Bool(true));
        assert_eq!(run("let nil = nil; true").unwrap().0, Value::Bool(true));
        assert!(run("let nil = true; false").is_err());
        assert!(run("let false = true").is_err());
        // TODO mutability
    }
}
