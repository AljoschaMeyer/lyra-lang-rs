use std::borrow::Borrow;
use std::rc::Rc;

use gc::{Gc, GcCell};
use rug::Rational;
use ref_thread_local::RefThreadLocal;

use super::gc_foreign::OrdMap;
use super::syntax::{Meta, Expression, _Expression, Statement, _Statement, Pattern, _Pattern, Patterns, FunLiteral, BinOp};
use super::builtins;

/// Runtime representation of an arbitrary lyra value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Trace, Finalize)]
pub enum Value {
    Nil,
    Bool(bool),
    Num(#[unsafe_ignore_trace] Rational),
    Fun(_Fun),
}

// TODO impl (Partial)Ord rather than deriving?

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Trace, Finalize)]
pub enum _Fun {
    Lyra {
        env: GcCell<Environment>, // interior mutability to enable recursive functions
        #[unsafe_ignore_trace]
        args: Box<[Pattern]>,
        #[unsafe_ignore_trace]
        body: Rc<Option<Statement>>,
    },
    Native0(fn() -> Result<Value, (Value, _Reason)>),
    Native1(fn(Value) -> Result<Value, (Value, _Reason)>),
    Native2(fn(Value, Value) -> Result<Value, (Value, _Reason)>),
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
    },
    NonFunApplication(Value),
    TypeError {
        expected: &'static str,
        got: Value,
        name: &'static str,
        index: usize,
    }
}

/// What a refutable pattern can expect.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum RefutationKind {
    Nil,
    Bool(bool),
    Num(Rational),
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
pub struct Environment(pub OrdMap<String, Binding>);

impl Environment {
    /// Return the starting environment for a new program run.
    pub fn toplevel() -> Environment {
        RefThreadLocal::borrow(&builtins::TOPLEVEL).clone()
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
            Binding::Mutable(val_cell) => GcCell::borrow(&val_cell).clone(),
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

    // Set the environment of a function.
    pub fn set_fun_env(&self, id: &str, new_env: Environment) {
        match self.0.get(id) {
            Some(Binding::Immutable(Value::Fun(_Fun::Lyra { ref env, .. }))) => {
                *env.borrow_mut() = new_env;
            }
            _ => panic!(), // don't call this function if this case would be reached...
        }
    }
}

pub fn truthy(val: &Value) -> bool {
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
pub fn evaluate(exp: &Expression, env: &Environment) -> Eval {
    match exp.0 {
        _Expression::Id(ref id) => Eval::Default(env.lookup(id)),
        _Expression::Nil => Eval::Default(Value::Nil),
        _Expression::Bool(v) => Eval::Default(Value::Bool(v)),
        _Expression::Num(ref v) => Eval::Default(Value::Num(v.clone())),
        _Expression::Land(ref left, ref right) => {
            evaluate(left, env).and_then(|val| {
                if truthy(&val) {
                    evaluate(right, env).map(|val| Value::Bool(truthy(&val)))
                } else {
                    Eval::Default(Value::Bool(false))
                }
            })
        }
        _Expression::Lor(ref left, ref right) => {
            evaluate(left, env).and_then(|val| {
                if truthy(&val) {
                    Eval::Default(Value::Bool(true))
                } else {
                    evaluate(right, env).map(|val| Value::Bool(truthy(&val)))
                }
            })
        }
        _Expression::BinOp(ref left, op, ref right) => {
            evaluate(left, env).and_then(|val_left | {
                evaluate(right, env).and_then(|val_right| {
                    match op {
                        BinOp::Eq => Eval::Default(builtins::eq(val_left, val_right).unwrap()),
                        BinOp::Neq => Eval::Default(builtins::neq(val_left, val_right).unwrap()),
                        BinOp::Lt => Eval::Default(builtins::lt(val_left, val_right).unwrap()),
                        BinOp::Lte => Eval::Default(builtins::lte(val_left, val_right).unwrap()),
                        BinOp::Gt => Eval::Default(builtins::gt(val_left, val_right).unwrap()),
                        BinOp::Gte => Eval::Default(builtins::gte(val_left, val_right).unwrap()),
                    }
                })
            })
        }
        _Expression::Not(ref inner) => {
            evaluate(inner, env).map(|val| builtins::is_not_truthy(val).unwrap())
        }
        _Expression::If(ref cond, ref then, ref else_) => {
            evaluate(cond, env).and_then(|cond_val| if truthy(&cond_val) {
                match then.borrow() {
                    Some(then) => exec(then, env.clone()).into(),
                    None => Eval::Default(Value::Nil),
                }
            } else {
                match else_.borrow() {
                    Some(else_stmt) => exec(else_stmt, env.clone()).into(),
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
                            match body.borrow() {
                                Some(body) => {
                                    match exec(body, env.clone()).into() {
                                        Eval::Default(val) => last_loop_val = val,
                                        Eval::Error(e, r) => return Eval::Error(e, r),
                                        Eval::Return(val) => return Eval::Return(val),
                                        Eval::Break(val) => return Eval::Default(val),
                                    }
                                }
                                None => last_loop_val = Value::Nil,
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
        _Expression::Try(ref try_body, ref catch_pat, ref catch_body) => {
            match try_body.borrow() {
                None => Eval::Default(Value::Nil),
                Some(try_body) => {
                    match exec(try_body, env.clone()).into() {
                        Eval::Default(val) => return Eval::Default(val),
                        Eval::Return(val) => return Eval::Return(val),
                        Eval::Break(val) => return Eval::Default(val),
                        Eval::Error(e, r) => {
                            match destructure(catch_pat, &e, env.clone()) {
                                Exec::Default(_, inner_env) => {
                                    match catch_body.borrow() {
                                        Some(catch_body) => return exec(catch_body, inner_env).into(),
                                        None => Eval::Default(Value::Nil),
                                    }
                                },
                                Exec::Error(_, _) => return Eval::Error(e, r),
                                _ => unreachable!(), // destructure can't return or break
                            }
                        }
                    }
                }
            }
        }
        _Expression::Case(ref exp, ref branches) => {
            evaluate(exp, &env).and_then(|val| {
                for (ref pats, ref body) in branches.iter() {
                    match destructure_patterns(pats, &val, env.clone()) {
                        None => {} // patterns did not match, just try the next branch
                        Some(inner_body) => {
                            return inner_body.and_then(|_, inner_env| {
                                match body.borrow() {
                                    Some(body) => exec(body, inner_env),
                                    None => Exec::Default(Value::Nil, inner_env),
                                }
                            }).into();
                        }
                    }
                }

                // No branch matched, return Nil by default.
                Eval::Default(Value::Nil)
            })
        }
        _Expression::Loop(ref matchee, ref branches) => {
            let mut last_loop_val = Value::Nil;
            'outer: loop {
                match evaluate(matchee, &env) {
                    Eval::Default(val) => {
                        for (ref pats, ref body) in branches.iter() {
                            match destructure_patterns(pats, &val, env.clone()) {
                                None => {} // patterns did not match, just try the next branch
                                Some(Exec::Error(e, r)) => return Eval::Error(e, r),
                                Some(Exec::Return(val)) => return Eval::Return(val),
                                Some(Exec::Break(val)) => return Eval::Default(val),
                                Some(Exec::Default(_, inner_env)) => {
                                    match body.borrow() {
                                        Some(body) => {
                                            match exec(body, inner_env) {
                                                Exec::Default(val, _) => {
                                                    last_loop_val = val;
                                                    continue 'outer; // begin next iteration of the lyra loop
                                                },
                                                Exec::Error(e, r) => return Eval::Error(e, r),
                                                Exec::Return(val) => return Eval::Return(val),
                                                Exec::Break(val) => return Eval::Default(val),
                                            }
                                        }
                                        None => {
                                            last_loop_val = Value::Nil;
                                            continue 'outer; // begin next iteration of the lyra loop
                                        }
                                    }
                                }
                            }
                        }

                        // No branch matched, the result of the previous loop run.
                        return Eval::Default(last_loop_val);
                    }
                    Eval::Error(e, r) => return Eval::Error(e, r),
                    Eval::Return(val) => return Eval::Return(val),
                    Eval::Break(val) => return Eval::Break(val),
                }
            }
        }
        _Expression::Application(ref fun, ref args) => {
            // Evaluate all arguments
            let mut arg_vals = Vec::new();
            loop {
                match args.get(arg_vals.len()) {
                    None => break,
                    Some(arg_exp) => {
                        match evaluate(arg_exp, env) {
                            Eval::Default(evaluated_arg) => arg_vals.push(evaluated_arg),
                            Eval::Error(e, r) => return Eval::Error(e, r),
                            Eval::Break(val) => return Eval::Break(val),
                            Eval::Return(val) => return Eval::Return(val),
                        }
                    }
                }
            }

            match evaluate(fun, &env) {
                Eval::Default(fun_val) => {
                    match fun_val {
                        Value::Fun(_Fun::Lyra { ref env, args: ref arg_patterns, ref body }) => {
                            let closed_env = env.borrow();

                            let mut inner_env = closed_env.clone();
                            for (i, pat) in arg_patterns.iter().enumerate() {
                                inner_env = match destructure(
                                    pat,
                                    arg_vals.get(i).unwrap_or(&Value::Nil),
                                    inner_env.clone()
                                ) {
                                    Exec::Default(_, new_env) => new_env,
                                    Exec::Error(e, r) => return Eval::Error(e, r),
                                    _ => unreachable!(), // destructure can't return or break
                                };
                            }

                            match body.borrow() {
                                Some(body) => return exec(body, inner_env).into(),
                                None => Eval::Default(Value::Nil),
                            }
                        }
                        Value::Fun(_Fun::Native0(f)) => {
                            match f() {
                                Ok(ret) => return Eval::Default(ret),
                                Err((e, r)) => return Eval::Error(e, Reason(r, fun.1.clone())),
                            }
                        }
                        Value::Fun(_Fun::Native1(f)) => {
                            match f(arg_vals.get(0).unwrap_or(&Value::Nil).clone()) {
                                Ok(ret) => return Eval::Default(ret),
                                Err((e, r)) => return Eval::Error(e, Reason(r, fun.1.clone())),
                            }
                        }
                        Value::Fun(_Fun::Native2(f)) => {
                            match f(
                                arg_vals.get(0).unwrap_or(&Value::Nil).clone(),
                                arg_vals.get(1).unwrap_or(&Value::Nil).clone()
                            ) {
                                Ok(ret) => return Eval::Default(ret),
                                Err((e, r)) => return Eval::Error(e, Reason(r, fun.1.clone())),
                            }
                        }
                        _ => return Eval::Error(RefThreadLocal::borrow(&builtins::ERR_NOT_A_FUNCTION).clone(), Reason(_Reason::NonFunApplication(fun_val), exp.1.clone())),
                    }
                }
                Eval::Error(e, r) => return Eval::Error(e, r),
                _ => unreachable!() // Functions either return a value via Eval::Default or they throw
            }
        }
        _Expression::Fun(FunLiteral(ref args, ref body)) => {
            return Eval::Default(Value::Fun(_Fun::Lyra {
                env: GcCell::new(env.clone()),
                args: args.clone(),
                body: body.clone(),
            }));
        }
    }
}

/// Executes a statement (syntax), returning the new environment (semantics) (and modifies the old
/// environment in case of (nested) assignments), as well as the resulting value.
pub fn exec(stmt: &Statement, mut env: Environment) -> Exec {
    match stmt.0 {
        _Statement::Chain(ref fst, ref snd) => {
            exec(fst, env).and_then(|_, snd_env| exec(snd, snd_env))
        }
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
        _Statement::Rec(ref recs) => {
            for rec in recs.iter() {
                let fun_val = match evaluate(&Expression(_Expression::Fun(rec.1.clone()), Meta::none()), &env) {
                    Eval::Default(val) => val,
                    _ => unreachable!(), // evaluating a function literal can't fail or exit early
                };

                env = env.insert(rec.0.to_string(), fun_val, false);
            }

            for rec in recs.iter() {
                env.set_fun_env(&rec.0, env.clone());
            }

            Exec::Default(Value::Nil, env)
        }
    }
}

/// Returns a new environment, adding bindings by destructuring the value according to the pattern.
pub fn destructure(Pattern(pat, meta): &Pattern, val: &Value, env: Environment) -> Exec {
    match pat {
        _Pattern::Blank => Exec::Default(Value::Nil, env),

        _Pattern::Nil => {
            match val {
                Value::Nil => Exec::Default(Value::Nil, env),
                _ => Exec::Error(RefThreadLocal::borrow(&builtins::ERR_REFUTED_NIL).clone(), Reason(_Reason::Refuted {
                    expected: RefutationKind::Nil,
                    actual: val.clone(),
                }, meta.clone())),
            }
        }

        _Pattern::Bool(b) => {
            match val {
                Value::Bool(b2) if b == b2 => Exec::Default(Value::Nil, env),
                _ => Exec::Error(RefThreadLocal::borrow(&builtins::ERR_REFUTED_BOOL).clone(), Reason(_Reason::Refuted {
                    expected: RefutationKind::Bool(*b),
                    actual: val.clone(),
                }, meta.clone())),
            }
        }

        _Pattern::Num(n) => {
            match val {
                Value::Num(n2) if n == n2 => Exec::Default(Value::Nil, env),
                _ => Exec::Error(RefThreadLocal::borrow(&builtins::ERR_REFUTED_NUM).clone(), Reason(_Reason::Refuted {
                    expected: RefutationKind::Num(n.clone()),
                    actual: val.clone(),
                }, meta.clone())),
            }
        }

        _Pattern::Id { id, mutable } => {
            Exec::Default(Value::Nil, env.insert(id.to_string(), val.clone(), *mutable))
        }
    }
}

/// Like `destructure`, but for `Patterns`. Returns `None` if nothing matched.
pub fn destructure_patterns(Patterns(ref pats, ref guard, _): &Patterns, val: &Value, env: Environment) -> Option<Exec> {
    for pat in pats.iter() {
        if let Exec::Default(val, inner_env) = destructure(pat, val, env.clone()) {
            match guard {
                None => return Some(Exec::Default(val, inner_env)),
                Some(ref guard_exp) => {
                    match evaluate(guard_exp, &inner_env) {
                        Eval::Default(guard_val) => {
                            if truthy(&guard_val) {
                                return Some(Exec::Default(val, inner_env));
                            }
                            // else just continue with the next pattern
                        }
                        Eval::Error(e, r) => return Some(Exec::Error(e, r)),
                        Eval::Return(val) => return Some(Exec::Return(val)),
                        Eval::Break(val) => return Some(Exec::Break(val)),
                    }
                }
            }
        }
        // else we didn't match, just continue (destructure can't return or break, err just means no match)
    }

    // No inner pattern matched.
    return None;
}
