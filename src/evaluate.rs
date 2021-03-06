// use std::sync::Arc;
//
// use im::{Vector, OrdSet, OrdMap, HashMap};
//
// use super::syntax::{self, *};
// use super::value::{self, *};
// use super::errors;
// use super::builtins;
//
// /// All evaluation happens in an environment that provides the names for free variables.
// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
// pub struct Env {
//     bindings: HashMap<Id, Value>,
//     parent: Option<Arc<Env>>,
//     args: Vector<Value>,
//     rec: Id,
// }
//
// impl Env {
//     pub fn root() -> Arc<Env> {
//         Arc::new(Env {
//             bindings: builtins::top_level(),
//             parent: None,
//             args: Vector::new(),
//             rec: Id(String::new()),
//         })
//     }
//
//     fn new_child(parent: Arc<Env>, rec: Id) -> Env {
//         Env {
//             bindings: HashMap::new(),
//             parent: Some(parent),
//             args: Vector::new(),
//             rec,
//         }
//     }
//
//     fn get(&self, id: &Id) -> Option<Option<Value>> {
//         if self.rec == *id {
//             return Some(None);
//         } else {
//             match self.bindings.get(id) {
//                 Some(val) => return Some(Some(val.clone())),
//                 None => {
//                     match self.parent {
//                         Some(ref parent) => return parent.get(id),
//                         None => return None,
//                     }
//                 }
//             }
//         }
//     }
//
//     fn extend_by_pattern(&self, pat: &Pattern, val: &Value) -> Env {
//         let mut base = self.clone();
//
//         match pat {
//             Pattern::Id(id, _) => {
//                 base.bindings.insert(id.clone(), val.clone());
//                 base
//             }
//             Pattern::Map(ref ids) => {
//                 match val {
//                     Value::Map(ref map) => {
//                         for (id, _) in ids.iter() {
//                             if let Some(value) = map.get(&id.0.as_str().into()) {
//                                 base.bindings.insert(id.clone(), value.clone());
//                             }
//                         }
//
//                         base
//                     }
//                     _ => base,
//                 }
//             },
//         }
//     }
//
//     fn extend_by_id(&self, id: &Id, val: &Value) -> Env {
//         let mut base = self.clone();
//         base.bindings.insert(id.clone(), val.clone());
//         base
//     }
// }
//
// /// The result of a (potentially partial) evaluation attempt.
// pub enum Evaluation {
//     /// The expression evaluated to this valect.
//     Yay(Value),
//     /// The expression threw this valect.
//     Threw(Value),
//     /// TODO Partial evaluation (for debugging)
//     Paused
// }
// use self::Evaluation::*;
//
// /// Short-circuiting evaluation similar to the std `try!` macro. Evaluate to the valect in
// /// a `Yay`, and directly returns on `Threw` or `Paused`.
// macro_rules! try_eval {
//     ($expr:expr) => (match $expr {
//         Evaluation::Yay(val) => val,
//         Evaluation::Threw(val) => return Evaluation::Threw(val),
//         Evaluation::Paused => return Evaluation::Paused,
//     });
//     ($expr:expr,) => (try!($expr));
// }
//
// /// Evaluate an expression inside an environment. Mutates the environment by adding bindings
// /// for each let expression.
// pub fn evaluate(exp: &Expression, env: Arc<Env>, self_fn: Option<syntax::Fun>) -> Evaluation {
//     match exp.0 {
//         _Expression::Nil => Yay(().into()),
//         _Expression::Args => Yay(env.args.clone().into()),
//         _Expression::Bool(inner) => Yay(inner.into()),
//         _Expression::Int(ref inner) => Yay(inner.into()),
//         _Expression::Char(inner) => Yay(inner.into()),
//         _Expression::String(ref inner) => Yay(inner.as_str().into()),
//         _Expression::Sequence(ref inners) => {
//             let mut seq = Vector::new();
//             for inner in inners.iter() {
//                 seq.push_back(try_eval!(evaluate(inner, env.clone(), self_fn)));
//             }
//             Yay(seq.into())
//         }
//         _Expression::Set(Set(ref inners)) => {
//             let mut set = OrdSet::new();
//             for inner in inners.iter() {
//                 set.insert(try_eval!(evaluate(inner, env.clone(), self_fn)));
//             }
//             Yay(set.into())
//         }
//         _Expression::Map(ref inners) => {
//             let mut map = OrdMap::new();
//             for inner in inners.iter() {
//                 map.insert(try_eval!(evaluate(&inner.0, env.clone(), self_fn)), try_eval!(evaluate(&inner.1, env.clone(), self_fn)));
//             }
//             Yay(map.into())
//         }
//         _Expression::If(If {ref cond, ref then, ref else_}) => {
//             let cond_evaluated = try_eval!(evaluate(cond, env.clone(), self_fn));
//
//             if cond_evaluated.truthyness() {
//                 evaluate(then, env, self_fn)
//             } else {
//                 else_.as_ref().map_or(Evaluation::Yay(Value::Nil), |e| evaluate(e, env, self_fn))
//             }
//         }
//         _Expression::Land(ref left, ref right) => {
//             let left_evaluated = try_eval!(evaluate(left, env.clone(), self_fn));
//             if left_evaluated.truthyness() {
//                 Evaluation::Yay(try_eval!(evaluate(right, env, self_fn)).truthyness().into())
//             } else {
//                 Evaluation::Yay(false.into())
//             }
//         }
//         _Expression::Lor(ref left, ref right) => {
//             let left_evaluated = try_eval!(evaluate(left, env.clone(), self_fn));
//             if left_evaluated.truthyness() {
//                 Evaluation::Yay(true.into())
//             } else {
//                 Evaluation::Yay(try_eval!(evaluate(right, env, self_fn)).truthyness().into())
//             }
//         }
//         _Expression::Throw(Throw(ref inner)) => Threw(try_eval!(evaluate(inner, env, self_fn))),
//         _Expression::Try(Try {ref to_try, ref caught, ref catcher}) => {
//             match evaluate(to_try, env.clone(), self_fn) {
//                 Evaluation::Yay(yay) => Evaluation::Yay(yay),
//                 Evaluation::Paused => Evaluation::Paused,
//                 Threw(exception) => {
//                     let env = env.extend_by_pattern(caught, &exception);
//                     evaluate(catcher, Arc::new(env), self_fn)
//                 },
//             }
//         }
//         _Expression::Pause(Pause {ref cond, ref continuing}) => {
//             // This does not pause, it just evaluates the condition and then the continuation.
//             if let Some(cond) = cond.as_ref() {
//                 let _ = try_eval!(evaluate(cond, env.clone(), self_fn));
//             }
//             evaluate(continuing, env, self_fn)
//         },
//         _Expression::Id(ref id) => {
//             match env.get(id) {
//                 Some(Some(val)) => Evaluation::Yay(val),
//                 Some(None) => Evaluation::Yay(Value::Fun(value::_Fun::LyraLiteral(syntax::Fun(self_fn.unwrap().0.clone()), env.clone()))),
//                 None => Evaluation::Threw(errors::free_identifier(id, &exp.1)),
//             }
//         },
//         _Expression::Let(Let::Let {ref lhs, ref rhs, ref continuing}) => {
//             let val = try_eval!(evaluate(rhs, env.clone(), self_fn));
//             let env = env.extend_by_pattern(lhs, &val);
//             evaluate(continuing, Arc::new(env), self_fn)
//         },
//         _Expression::Let(Let::Fun {ref lhs, ref rhs, ref continuing}) => {
//             let val = Value::Fun(value::_Fun::LyraLiteral(rhs.0.clone(), Arc::new(Env::new_child(env.clone(), lhs.clone()))));
//             let env = env.extend_by_id(lhs, &val);
//             evaluate(continuing, Arc::new(env), self_fn)
//         },
//         _Expression::Fun(ref fun) => {
//             Yay(Value::Fun(value::_Fun::LyraLiteral(fun.clone(), Arc::new(Env::new_child(env, Id(String::new()))))))
//         },
//         _Expression::App(App {ref fun, ref args, tail}) => {
//             // TODO trampoline if tail
//             let fun_val = try_eval!(evaluate(fun, env.clone(), self_fn));
//
//             let mut arg_seq = Vector::new();
//             for arg in args.iter() {
//                 arg_seq.push_back(try_eval!(evaluate(arg, env.clone(), self_fn)));
//             }
//
//             match fun_val {
//                 Value::Fun(value::_Fun::LyraLiteral(fun_syntax, fun_env)) => {
//                     let mut eval_env = fun_env.clone();
//                     eval_env.args = arg_seq;
//
//                     for (i, id) in fun_syntax.0.args.iter().enumerate() {
//                         eval_env = eval_env.extend_by_id(&id.0, eval_env.args.get(i).unwrap_or(&Value::Nil));
//                     }
//
//                     evaluate(&fun_syntax.0.body, Arc::new(eval_env) Some(fun_syntax.clone()))
//                 }
//                 Value::Fun(value::_Fun::Rust(native_fun)) => {
//                     match native_fun(arg_seq) {
//                         Ok(val) => Evaluation::Yay(val),
//                         Err(val) => Evaluation::Threw(val),
//                     }
//                 },
//                 _ => Evaluation::Threw(errors::non_fun_call(&fun_val, &exp.1)),
//             }
//         },
//     }
// }
