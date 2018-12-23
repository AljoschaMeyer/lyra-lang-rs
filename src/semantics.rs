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

/// The outcome of executing a statement.
pub enum Exec {
    /// The statement has been executed, the (possibly modified) environment is returned together with its value.
    Default(Value, Environment),
    /// For some reason (type errors, throw statements, etc.), an error value was produced.
    Error(Value, Reason),
    /// Executed a return statement.
    Return(Value),
    /// Executed a break statement.
    Break(Value),
}

impl Exec {
    pub fn from_eval(eval: Eval, env: Environment) -> Exec {
        match eval {
            Eval::Default(val) => Exec::Default(val, env),
            Eval::Error(e, r) => Exec::Error(e, r),
            Eval::Return(val) => Exec::Return(val),
            Eval::Break(val) => Exec::Break(val),
        }
    }
    
    fn and_then<F: FnOnce(Value, Environment) -> Exec>(self, f: F) -> Exec {
        match self {
            Exec::Default(val, env) => f(val, env),
            Exec::Error(val, r) => Exec::Error(val, r),
            Exec::Return(val) => Exec::Return(val),
            Exec::Break(val) => Exec::Break(val),
        }
    }
}

/// The outcome of evluating an expression.
pub enum Eval {
    Default(Value),
    Error(Value, Reason),
    /// Somewhere in the expression, a return statement has been evaluated (so if this Eval is
    /// part of executing a series of statements, the execution can short-circuit)
    Return(Value),
    /// Like `Return`, but for break statements.
    Break(Value),
}

impl Eval {
    fn map<F: FnOnce(Value) -> Value>(self, f: F) -> Eval {
        match self {
            Eval::Default(val) => Eval::Default(f(val)),
            Eval::Error(val, r) => Eval::Error(val, r),
            Eval::Return(val) => Eval::Return(val),
            Eval::Break(val) => Eval::Break(val),
        }
    }
    
    fn and_then<F: FnOnce(Value) -> Eval>(self, f: F) -> Eval {
        match self {
            Eval::Default(val) => f(val),
            Eval::Error(val, r) => Eval::Error(val, r),
            Eval::Return(val) => Eval::Return(val),
            Eval::Break(val) => Eval::Break(val),
        }
    }
}

impl From<Exec> for Eval {
    fn from(exec: Exec) -> Eval {
        match exec {
            Exec::Default(val, _) => Eval::Default(val),
            Exec::Error(e, r) => Eval::Error(e, r),
            Exec::Return(val) => Eval::Return(val),
            Exec::Break(val) => Eval::Break(val),
        }
    }
}

/// Turns an expression (syntax) into a value (semantics), looking up bindings in the given
/// environment (and modifying it in case of assignments nested in the expression).
///
/// `Err` if the evaluation throws, `Ok` otherwise.
pub fn evaluate(exp: &Expression, env: &Environment) -> Eval {
    // TODO trampoline tail-calls
    let _ = env;

    match exp.0 {
        _Expression::Id(ref id) => Eval::Default(env.lookup(id)),
        _Expression::Nil => Eval::Default(Value::Nil),
        _Expression::Bool(v) => Eval::Default(Value::Bool(v)),
        _Expression::Land(ref left, ref right) => {
            match evaluate(left, env) {
                Eval::Default(ref val) if truthy(val) => {
                    evaluate(right, env).map(|val| Value::Bool(truthy(&val)))
                }
                _ => Eval::Default(Value::Bool(false)),
            }
        }
        _Expression::Lor(ref left, ref right) => {
            match evaluate(left, env) {
                Eval::Default(ref val) if truthy(val) => Eval::Default(Value::Bool(true)),
                _ => evaluate(right, env).map(|val| Value::Bool(truthy(&val))),
            }
        }
        _Expression::If(ref cond, ref then, ref else_) => {
            evaluate(cond, env).and_then(|cond_val| if truthy(&cond_val) {
                exec_many(&mut then.iter(), env.clone()).into()
            } else {
                match else_ {
                    Some(else_stmts) => exec_many(&mut else_stmts.iter(), env.clone()).into(),
                    None => Eval::Default(Value::Nil),
                }
            })
        }
        _Expression::While(ref cond, ref body) => {
            let mut last_loop_val = Value::Nil;
            loop {
                match evaluate(cond, env) {
                    Eval::Default(val) => {
                        if truthy(&val) {
                            match exec_many(&mut body.iter(), env.clone()).into() {
                                Eval::Default(val) => last_loop_val = val,
                                Eval::Error(e, r) => return Eval::Error(e, r),
                                Eval::Return(val) => return Eval::Return(val),
                                Eval::Break(val) => return Eval::Default(val),
                            }
                        } else {
                            return Eval::Default(last_loop_val);
                        }
                    }
                    Eval::Error(e, r) => return Eval::Error(e, r),
                    Eval::Return(val) => return Eval::Return(val),
                    Eval::Break(val) => return Eval::Break(val),
                }
            }
        }
    }
}

/// Executes a statement (syntax), returning the new environment (semantics) (and modifies the old
/// environment in case of (nested) assignments), as well as the resulting value.
///
/// `Err` if the execution throws, `Ok` otherwise.
pub fn exec(stmt: &Statement, env: Environment) -> Exec {
    match stmt.0 {
        _Statement::Exp(ref exp) => {
            Exec::from_eval(evaluate(exp, &env), env)
        }
        _Statement::Let(ref lhs, ref rhs) => {
            Exec::from_eval(evaluate(rhs, &env), env)
                .and_then(|val, env| destructure(lhs, &val, env))
        }
        _Statement::Assign(ref lhs, ref rhs) => {
            Exec::from_eval(evaluate(rhs, &env).map(|val| {
                env.assign(lhs, val);
                Value::Nil
            }), env)
        }
        _Statement::Throw(ref exp) => {
            Exec::from_eval(
                evaluate(exp, &env)
                    .and_then(|val| Eval::Error(val, Reason(_Reason::Thrown, exp.1.clone()))),
                env
            )
        }
        _Statement::Return(ref exp) => {
            Exec::from_eval(evaluate(exp, &env).and_then(|val| Eval::Return(val)), env)
        }
        _Statement::Break(ref exp) => {
            Exec::from_eval(evaluate(exp, &env).and_then(|val| Eval::Break(val)), env)
        }
    }
}

/// Execute multiple statements in sequence, threading the environments through, short-circuiting
/// upon throws, returns, and breaks.
///
/// For zero statements, this returns the environment unchanged and Ok(Value::Nil).
pub fn exec_many<'i, I>(stmts: &'i mut I, mut env: Environment) -> Exec
    where I: Iterator<Item = &'i Statement> {
        let mut val = Value::Nil;

        for stmt in stmts {
            match exec(stmt, env) {
                Exec::Default(new_val, new_env) => {
                    val = new_val;
                    env = new_env;
                }
                Exec::Error(e, r) => return Exec::Error(e, r),
                Exec::Return(val) => return Exec::Return(val),
                Exec::Break(val) => return Exec::Break(val),
            }
        }

        return Exec::Default(val, env);
}

/// Returns a new environment, adding bindings by destructuring the value according to the pattern.
///
/// `Err` if the pattern is refuted, `Ok(Value::Nil)` otherwise.
pub fn destructure(Pattern(pat, meta): &Pattern, val: &Value, env: Environment) -> Exec {
    match pat {
        _Pattern::Blank => Exec::Default(Value::Nil, env),
        
        _Pattern::Nil => {
            match val {
                Value::Nil => Exec::Default(Value::Nil, env),
                _ => Exec::Error(builtins::ERR_REFUTED_NIL.borrow().clone(), Reason(_Reason::Refuted {
                    expected: RefutationKind::Nil,
                    actual: val.clone(),
                }, meta.clone())),
            }
        }
        
        _Pattern::Bool(b) => {
            match val {
                Value::Bool(b2) if b == b2 => Exec::Default(Value::Nil, env),
                _ => Exec::Error(builtins::ERR_REFUTED_BOOL.borrow().clone(), Reason(_Reason::Refuted {
                    expected: RefutationKind::Bool(*b),
                    actual: val.clone(),
                }, meta.clone())),
            }
        }
        
        _Pattern::Id { id, mutable } => {
            Exec::Default(Value::Nil, env.insert(id.to_string(), val.clone(), *mutable))
        }
    }
}