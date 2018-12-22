use gc::{Gc, GcCell};
use ref_thread_local::RefThreadLocal;

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
    /// Panics if the id is unbound, but that's a static parse-error anyways.
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
    
    /// Assign a new value to a mutable binding.
    ///
    /// Panics if the id is unbound or immutable, but that's a static parse-error anyways.
    pub fn assign(&self, id: &str, val: Value) {
        match self.0.get(id).unwrap() {
            Binding::Immutable(_) => panic!("Tried to assign to immutable binding"),
            Binding::Mutable(val_cell) => *val_cell.borrow_mut() = val,
        }
    }
}

fn truthy(val: &Value) -> bool {
    match val {
        Value::Nil | Value::Bool(false) => false,
        _ => true,
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
        _Expression::Land(ref left, ref right) => {
            if truthy(&evaluate(left, env)?) {
                Ok(Value::Bool(truthy(&evaluate(right, env)?)))
            } else {
                Ok(Value::Bool(false))
            }
        }
        _Expression::Lor(ref left, ref right) => {
            if truthy(&evaluate(left, env)?) {
                Ok(Value::Bool(true))
            } else {
                Ok(Value::Bool(truthy(&evaluate(right, env)?)))
            }
        }
    }
}

/// Executes a statement (syntax), returning the new environment (semantics) (and modifies the old
/// environment in case of (nested) assignments), as well as the resulting value.
///
/// `Err` if the execution throws, `Ok` otherwise.
pub fn exec(stmt: &Statement, env: Environment) -> Result<(Value, Environment), (Value, Reason)> {
    match stmt.0 {
        _Statement::Exp(ref exp) => {
            evaluate(exp, &env).map(|val| (val, env))
        }
        _Statement::Let(ref lhs, ref rhs) => {
            evaluate(rhs, &env).and_then(|val| destructure(lhs, &val, env))
        }
        _Statement::Assign(ref lhs, ref rhs) => {
            let val = evaluate(rhs, &env)?;
            env.assign(lhs, val);
            Ok((Value::Nil, env))
        }
    }
}

/// Execute multiple statements in sequence, threading the environments through, short-circuiting
/// upon throws.
///
/// For zero statements, this returns the environment unchanged and Ok(Value::Nil).
pub fn exec_many<'i, I>(stmts: &'i mut I, mut env: Environment) -> Result<(Value, Environment), (Value, Reason)>
    where I: Iterator<Item = &'i Statement> {
        let mut val = Value::Nil;

        for stmt in stmts {
            match exec(stmt, env) {
                Ok((new_val, new_env)) => {
                    val = new_val;
                    env = new_env;
                }
                Err(err) => return Err(err),
            }
        }

        return Ok((val, env));
}

/// Returns a new environment, adding bindings by destructuring the value according to the pattern.
///
/// `Err` if the pattern is refuted, `Ok(Value::Nil)` otherwise.
pub fn destructure(Pattern(pat, meta): &Pattern, val: &Value, env: Environment) -> Result<(Value, Environment), (Value, Reason)> {
    match pat {
        _Pattern::Blank => Ok((Value::Nil, env)),
        
        _Pattern::Nil => {
            match val {
                Value::Nil => Ok((Value::Nil, env)),
                _ => Err((builtins::ERR_REFUTED_NIL.borrow().clone(), Reason(_Reason::Refuted {
                    expected: RefutationKind::Nil,
                    actual: val.clone(),
                }, meta.clone())))
            }
        }
        
        _Pattern::Bool(b) => {
            match val {
                Value::Bool(b2) if b == b2 => Ok((Value::Nil, env)),
                _ => Err((builtins::ERR_REFUTED_BOOL.borrow().clone(), Reason(_Reason::Refuted {
                    expected: RefutationKind::Bool(*b),
                    actual: val.clone(),
                }, meta.clone())))
            }
        }
        
        _Pattern::Id { id, mutable } => {
            Ok((Value::Nil, env.insert(id.to_string(), val.clone(), *mutable)))
        }
    }
}