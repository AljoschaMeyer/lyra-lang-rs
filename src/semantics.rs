use gc::{Gc, GcCell};

use super::gc_foreign::OrdMap;
use super::syntax::{Meta, Expression, _Expression, Statement, _Statement, Pattern, _Pattern};
use super::builtins;

/// Runtime representation of an arbitrary lyra value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Trace, Finalize)]
pub enum Value {
    Nil,
    Bool(bool),
    // Fun {
    //     env: GcCell<Environment>, // interior mutability to enable recursive functions
    //     #[unsafe_ignore_trace]
    //     args: Box<[Pattern]>,
    //     #[unsafe_ignore_trace]
    //     body: Box<[Statement]>,
    // }
}

/// Metadata about an exception. Not accessible to the program itself, only used for helpful
/// diagnostics for the programmer.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Reason(_Reason, Meta);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum _Reason {
    Thrown,
    Refuted {
        expected: RefutationKind,
        actual: Value
    }
}

/// What a refutable pattern can expect.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum RefutationKind {
    Nil,
    Bool(bool),
}

/// The environments need to distinguish whether the values they store are for mutable or immutable
/// bindings. Mutable values are implemented via interior mutability and extra indirection.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Trace, Finalize)]
pub enum Binding {
    Immutable(Value),
    Mutable(Gc<GcCell<Value>>),
}

/// The execution environment, this is the internal program state that is modified by statements.
/// Serves to look up bindings by name.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Trace, Finalize)]
pub struct Environment(OrdMap<String, Binding>);

impl Environment {
    /// Return the starting environment for a new program run.
    pub fn toplevel() -> Environment {
        Environment::empty() // TODO actually implement this
    }

    /// Return an empty environment.
    pub fn empty() -> Environment {
        Environment(OrdMap::new())
    }

    /// Look up an identifier in the environment.
    ///
    /// Since there's a static check that prohibits free variables, this can never fail at runtime.
    pub fn lookup(&self, id: &str) -> Value {
        match self.0.get(id).unwrap() {
            Binding::Immutable(val) => val.clone(),
            Binding::Mutable(val_cell) => val_cell.borrow().clone(),
        }
    }
    
    /// Create a new environment with the given value bound to the given id.
    pub fn insert(&self, id: String, val: Value, mutable: bool) -> Environment {
        let binding = if mutable {
            Binding::Mutable(Gc::new(GcCell::new(val)))
        } else {
            Binding::Immutable(val)
        };
        
        Environment(self.0.update(id, binding))
    }
}

/// Turns an expression (syntax) into a value (semantics), looking up bindings in the given
/// environment (and modifying it in case of assignments nested in the expression).
///
/// `Err` if the evaluation throws, `Ok` otherwise.
pub fn evaluate(exp: &Expression, env: &Environment) -> Result<Value, (Value, Reason)> {
    // TODO trampoline tail-calls
    let _ = env;

    match exp.0 {
        _Expression::Id(ref id) => Ok(env.lookup(id)),
        _Expression::Nil => Ok(Value::Nil),
        _Expression::Bool(v) => Ok(Value::Bool(v)),
    }
}

/// Executes a statement (syntax), returning the new environment (semantics) (and modifies the old
/// environment in case of (nested) assignments), as well as the resulting value.
///
/// `Err` if the execution throws, `Ok` otherwise.
pub fn exec(stmt: &Statement, env: Environment) -> (Environment, Result<Value, (Value, Reason)>) {
    // TODO can this return Result<(Value, Environment), (Value, Reason)>?
    match stmt.0 {
        _Statement::Exp(ref exp) => {
            let eval = evaluate(exp, &env);
            return (env, eval);
        }
        _Statement::Let(ref lhs, ref rhs) => {
            match evaluate(rhs, &env) {
                Ok(rhs_val) => destructure(lhs, &rhs_val, env),
                Err(thrown) => (env, Err(thrown)),
            }
        }
    }
}

/// Execute multiple statements in sequence, threading the environments through, short-circuiting
/// upon throws.
///
/// For zero statements, this returns the environment unchanged and Ok(Value::Nil).
pub fn exec_many<'i, I>(stmts: &'i mut I, mut env: Environment) -> (Environment, Result<Value, (Value, Reason)>)
    where I: Iterator<Item = &'i Statement> {
        let mut eval = Ok(Value::Nil);

        for stmt in stmts {
            let (new_env, new_eval) = exec(stmt, env);
            env = new_env;
            eval = new_eval;

            if eval.is_err() {
                return (env, eval);
            }
        }

        return (env, eval);
}

/// Returns a new environment, adding bindings by destructuring the value according to the pattern.
///
/// `Err` if the pattern is refuted, `Ok(Value::Nil)` otherwise.
pub fn destructure(Pattern(pat, meta): &Pattern, val: &Value, env: Environment) -> (Environment, Result<Value, (Value, Reason)>) {
    match pat {
        _Pattern::Blank => (env, Ok(Value::Nil)),
        
        _Pattern::Nil => {
            match val {
                Value::Nil => (env, Ok(Value::Nil)),
                _ => (env, Err((builtins::ERR_REFUTED_NIL.clone(), Reason(_Reason::Refuted {
                    expected: RefutationKind::Nil,
                    actual: val.clone(),
                }, meta.clone()))))
            }
        }
        
        _Pattern::Bool(b) => {
            match val {
                Value::Bool(b2) if b == b2 => (env, Ok(Value::Nil)),
                _ => (env, Err((builtins::ERR_REFUTED_BOOL.clone(), Reason(_Reason::Refuted {
                    expected: RefutationKind::Bool(*b),
                    actual: val.clone(),
                }, meta.clone()))))
            }
        }
        
        _Pattern::Id { id, mutable } => {
            (env.insert(id.to_string(), val.clone(), *mutable), Ok(Value::Nil))
        }
    }
}