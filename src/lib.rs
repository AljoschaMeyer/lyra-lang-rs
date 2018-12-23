#![feature(try_from)]

extern crate failure;
#[macro_use] extern crate failure_derive;
extern crate im_rc;
#[macro_use] extern crate ref_thread_local;
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
    use super::semantics::{Value, exec_many, Environment, Reason, Exec};

    fn run(src: &str) -> Result<Value, (Value, Reason)> {
        let mut p = Parser::new(src, Source::other());
        let program = p.p_program().unwrap();
        let eval = exec_many(&mut program.iter(), Environment::toplevel());
        assert!(p.end());
        
        match eval {
            Exec::Default(val, _) | Exec::Return(val) | Exec::Break(val) => Ok(val),
            Exec::Error(e, r) => Err((e, r))
        }
    }
    
    fn assert_syntax_err(src: &str) {
        let mut p = Parser::new(src, Source::other());
        let program = p.p_statements().unwrap();
        // TODO impl checks for unbound identifiers or assignment to immutable identifiers
        assert!(p.end());
    }

    #[test]
    fn test_nil() {
        assert_eq!(run("nil; nil").unwrap(), Value::Nil);
        assert_eq!(run("").unwrap(), Value::Nil);
    }

    #[test]
    fn test_bool() {
        assert_eq!(run(" true").unwrap(), Value::Bool(true));
        assert_eq!(run("false ").unwrap(), Value::Bool(false));
    }
    
    #[test]
    fn test_let() {
        assert_eq!(run("let x = true").unwrap(), Value::Nil);
        assert_eq!(run("let mut x = true").unwrap(), Value::Nil);
        assert_eq!(run("let _ = true").unwrap(), Value::Nil);
        assert_eq!(run("let _ = nil; true").unwrap(), Value::Bool(true));
        assert_eq!(run("let x = true; x").unwrap(), Value::Bool(true));
        assert_eq!(run("let mut x = true; x").unwrap(), Value::Bool(true));
        assert_eq!(run("let nil = nil; true").unwrap(), Value::Bool(true));
        assert!(run("let nil = true; false").is_err());
        assert!(run("let false = true").is_err());
        assert_eq!(run("let x = false; let x = true; x").unwrap(), Value::Bool(true));
    }
    
    #[test]
    fn test_assign() {
        assert_eq!(run("let mut x = true; x = false; x").unwrap(), Value::Bool(false));
        assert_eq!(run("let mut x = true; x = false").unwrap(), Value::Nil);
        assert_syntax_err("let x = true; x = false");
        assert_syntax_err("let mut x = true; let x = true; x = false");
    }
    
    #[test]
    fn test_land() {
        assert_eq!(run("true && true").unwrap(), Value::Bool(true));
        assert_eq!(run("true && false").unwrap(), Value::Bool(false));
        assert_eq!(run("false && true").unwrap(), Value::Bool(false));
        assert_eq!(run("false && false").unwrap(), Value::Bool(false));
        
        // TODO uncomment when applications are implemented
        // assert_eq!(run("false && halt()").unwrap(), Value::Bool(false));
    }
    
    #[test]
    fn test_lor() {
        assert_eq!(run("true || true").unwrap(), Value::Bool(true));
        assert_eq!(run("true || false").unwrap(), Value::Bool(true));
        assert_eq!(run("false || true").unwrap(), Value::Bool(true));
        assert_eq!(run("false || false").unwrap(), Value::Bool(false));
        
        // TODO uncomment when applications are implemented
        // assert_eq!(run("true && halt()").unwrap(), Value::Bool(true));
    }
    
    #[test]
    fn test_parens() {
        assert_eq!(run("false && false || true").unwrap(), Value::Bool(false));
        assert_eq!(run("false && (false || true)").unwrap(), Value::Bool(false));
        assert_eq!(run("(false && false) || true").unwrap(), Value::Bool(true));
    }
    
    #[test]
    fn test_if() {
        assert_eq!(run("if true { true }").unwrap(), Value::Bool(true));
        // assert_eq!(run("if true { true } else { halt() }").unwrap(), Value::Bool(true)); // TODO uncomment when applications are implemented
        assert_eq!(run("if false { false } else { true }").unwrap(), Value::Bool(true));
        assert_eq!(run("if true {} else { true }").unwrap(), Value::Nil);
        // assert_eq!(run("if false { halt() } else { true }").unwrap(), Value::Bool(true)); // TODO uncomment when applications are implemented
        assert_eq!(run("if false { true }").unwrap(), Value::Nil);
        assert_eq!(run("if nil { true }").unwrap(), Value::Nil);
    }
    
    #[test]
    fn test_throw() {
        assert_eq!(run("throw true; false").unwrap_err().0, Value::Bool(true));
    }
    
    #[test]
    fn test_return() {
        assert_eq!(run("return true; false").unwrap(), Value::Bool(true));
    }
    
    #[test]
    fn test_break() {
        assert_eq!(run("break true; false").unwrap(), Value::Bool(true));
    }
    
    #[test]
    fn test_while() {
        assert_eq!(run("while false { false }; true").unwrap(), Value::Bool(true));
        assert_eq!(run("while false {}").unwrap(), Value::Nil);
        assert_eq!(run("let mut x = true; while x { x = false; true }").unwrap(), Value::Bool(true));
        assert_eq!(run("while true { return false }").unwrap(), Value::Bool(false));
        assert_eq!(run("while true { return false }; true").unwrap(), Value::Bool(false));
        assert_eq!(run("while true { break false }").unwrap(), Value::Bool(false));
        assert_eq!(run("while true { break false }; true").unwrap(), Value::Bool(true));
    }
}
