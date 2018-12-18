use super::syntax::{Expression, _Expression, Statement, _Statement};

/// Runtime representation of an arbitrary lyra value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Trace, Finalize)]
pub enum Value {
    Nil,
    Bool(bool),
}

/// The execution environment, this is the internal program state that is modified by statements.
/// Serves to look up bindings by name.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Trace, Finalize)]
pub struct Environment;

/// Return the starting environment for a new program run.
pub fn initial_env() -> Environment {
    Environment
}

/// Turns an expression (syntax) into a value (semantics), looking up bindings in the given
/// environment (and modifying it in case of assignments nested in the expression).
///
/// `Err` if the evaluation throws, `Ok` otherwise.
pub fn evaluate(exp: &Expression, env: &Environment) -> Result<Value, Value> {
    // TODO trampoline tail-calls
    let _ = env;

    match exp.0 {
        _Expression::Nil => Ok(Value::Nil),
        _Expression::Bool(v) => Ok(Value::Bool(v)),
    }
}

/// Executes a statement (syntax), returning the new environment (semantics) (and modifies the old
/// environment in case of (nested) assignments), as well as the resulting value.
///
/// `Err` if the execution throws, `Ok` otherwise.
pub fn exec(stmt: &Statement, env: Environment) -> (Environment, Result<Value, Value>) {
    match stmt.0 {
        _Statement::Exp(ref exp) => {
            let eval = evaluate(exp, &env);
            return (env, eval);
        }
    }
}

/// Execute multiple statements in sequence, treading through the environments, short-circuiting
/// upon throws.
///
/// For zero statements, this returns the environment unchanged and Ok(Value::Nil).
pub fn exec_many<'i, I>(stmts: &'i mut I, mut env: Environment) -> (Environment, Result<Value, Value>)
    where I: Iterator<Item = &'i Statement> {
        let mut eval = Ok(Value::Nil);

        for stmt in stmts {
            let (new_env, new_eval) = exec(stmt, env);
            env = new_env;
            eval = new_eval;

            if eval.is_err() {
                return (env, eval)
            }
        }

        return (env, eval);
}
