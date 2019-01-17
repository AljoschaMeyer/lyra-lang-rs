#![feature(try_from)]

extern crate failure;
#[macro_use] extern crate failure_derive;
extern crate im_rc;
#[macro_use] extern crate ref_thread_local;
#[macro_use] extern crate gc_derive;
#[macro_use] extern crate gc;
extern crate rug;
extern crate ropey;

pub mod gc_foreign;
pub mod syntax;
pub mod syntax_checks;
pub mod parser;
pub mod semantics;
pub mod builtins;

#[cfg(test)]
mod tests {
    use super::parser::Parser;
    use super::syntax::Source;
    use super::syntax_checks::{Scope, check};
    use super::semantics::{Value, Environment, exec, Reason, Exec};

    fn run(src: &str) -> Result<Value, (Value, Reason)> {
        let mut p = Parser::new(src, Source::other());
        p.skip_ws();
        let program = p.p_statement().unwrap();
        let eval = exec(&program, Environment::toplevel());
        assert!(p.end());

        match eval {
            Exec::Default(val, _) | Exec::Return(val) | Exec::Break(val) => Ok(val),
            Exec::Error(e, r) => Err((e, r))
        }
    }

    fn assert_syntax_err(src: &str) {
        let mut p = Parser::new(src, Source::other());
        p.skip_ws();
        let program = p.p_statement().unwrap();

        let top_scope = Scope::from_env(&Environment::toplevel());
        assert!(p.end());
        assert!(check(top_scope, &program).is_err());
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
        assert_eq!(run("false && halt()").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_lor() {
        assert_eq!(run("true || true").unwrap(), Value::Bool(true));
        assert_eq!(run("true || false").unwrap(), Value::Bool(true));
        assert_eq!(run("false || true").unwrap(), Value::Bool(true));
        assert_eq!(run("false || false").unwrap(), Value::Bool(false));
        assert_eq!(run("true || halt()").unwrap(), Value::Bool(true));
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
        assert_eq!(run("if true { true } else { halt() }").unwrap(), Value::Bool(true));
        assert_eq!(run("if false { false } else { true }").unwrap(), Value::Bool(true));
        assert_eq!(run("if true {} else { true }").unwrap(), Value::Nil);
        assert_eq!(run("if false { halt() } else { true }").unwrap(), Value::Bool(true));
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

    #[test]
    fn test_try() {
        assert_eq!(run("try {} catch _ {}").unwrap(), Value::Nil);
        assert_eq!(run("try { true } catch _ {}").unwrap(), Value::Bool(true));
        assert_eq!(run("try { throw true; false } catch x { x }").unwrap(), Value::Bool(true));
        assert_eq!(run("try { throw true; false } catch false { false }").unwrap_err().0, Value::Bool(true));
        assert_eq!(run("try { let nil = false } catch _ { true }").unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_case() {
        assert_syntax_err("case nil { x | true -> { x }}");
        assert_eq!(run("case true {}").unwrap(), Value::Nil);
        assert_eq!(run("case true {_ -> {true}}").unwrap(), Value::Bool(true));
        assert_eq!(run("case true {true -> {}}").unwrap(), Value::Nil);
        assert_eq!(run("case true {true -> {true}}").unwrap(), Value::Bool(true));
        assert_eq!(run("case true {false -> {true}}").unwrap(), Value::Nil);
        assert_eq!(run("case false {true -> {true} false -> {false}}").unwrap(), Value::Bool(false));
        assert_eq!(run("case true {x -> {x}}").unwrap(), Value::Bool(true));
        assert_eq!(run("case true {x if true -> {x} _ -> {false}}").unwrap(), Value::Bool(true));
        assert_eq!(run("case true {true | nil -> {true} _ -> {}}").unwrap(), Value::Bool(true));
        assert_eq!(run("case nil {true | nil -> {false} _ -> {}}").unwrap(), Value::Bool(false));
        assert_eq!(run("case false {true | nil -> {nil} _ -> { false }}").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_loop() {
        assert_eq!(run("loop true {}").unwrap(), Value::Nil);
        assert_eq!(run("let mut x = true; loop x { nil -> {x = false; true} true -> {x = nil}}").unwrap(), Value::Bool(true));
        assert_eq!(run("loop true { _ -> { break true }}").unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_num() {
        assert_eq!(run("17 == 17.00").unwrap(), Value::Bool(true));
        assert_eq!(run("1_7 == 0x11").unwrap(), Value::Bool(true));

        assert_eq!(run("case 5 {50 / 10.0 -> { true } _ -> { false }}").unwrap(), Value::Bool(true));
    }

    #[test]
    #[should_panic(expected = "called the built-in `halt` function")]
    fn test_halt() {
        let _ = run("halt()");
    }

    #[test]
    fn test_fun() {
        assert_eq!(run("(x) -> { x }(true)").unwrap(), Value::Bool(true));
        assert_eq!(run("let mut x = false; (y) -> { x = y }(true); x").unwrap(), Value::Bool(true));
        assert_eq!(run("
let x = false;
let f = () -> {
    let mut x = false;
    () -> { x = true }()
};
x
").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_not() {
        assert_eq!(run("!nil").unwrap(), Value::Bool(true));
        assert_eq!(run("! false").unwrap(), Value::Bool(true));
        assert_eq!(run("!true").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_cmp() {
        assert_eq!(run("nil < false").unwrap(), Value::Bool(true));
        assert_eq!(run("false < true").unwrap(), Value::Bool(true));
    }

    // #[test]
    // fn test_tail_call_optimization() {
    //     // TODO replace these with functions that terminate
    //     let _ = run("rec diverge = (x) -> { diverge(bool_not(x)) }; diverge(true)"); // Must not crash (due to tail-call optimization), must go into an infinite loop.
    //     let _ = run("rec {
    //         foo = () -> { bar() }
    //         bar = () -> { foo() }
    //     }; foo()"); // Must not crash (due to tail-call optimization), must go into an infinite loop.
    // }
}
