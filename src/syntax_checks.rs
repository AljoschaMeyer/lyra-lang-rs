use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use super::syntax::*;
use super::semantics::Environment;

pub struct Scope {
    bindings: HashMap<String, bool>, // true for mutable, false for immutable
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn empty() -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    pub fn from_env(env: &Environment) -> Rc<RefCell<Scope>> {
        let scope = Scope::empty();

        for id in (env.0).0.keys() {
            scope.borrow_mut().binder(id, false);
        }

        scope
    }

    pub fn child(parent: Rc<RefCell<Scope>>) -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope {
            bindings: HashMap::new(),
            parent: Some(parent),
        }))
    }

    pub fn binder(&mut self, id: &str, mutable: bool) {
        self.bindings.insert(id.to_string(), mutable);
    }

    // Return true if the given id is not bound at all (i.e. a syntax error).
    pub fn free(&self, id: &str) -> bool {
        if self.bindings.contains_key(id) {
            return false;
        } else {
            match self.parent {
                None => return true,
                Some(ref p) => return p.borrow().free(id),
            }
        }
    }

    pub fn free_or_immutable(&self, id: &str) -> bool {
        match self.bindings.get(id) {
            Some(false) => return true,
            Some(true) => return false,
            None => {
                match self.parent {
                    None => return true,
                    Some(ref p) => return p.borrow().free_or_immutable(id),
                }
            }
        }
    }
}

// true if all variable usages are ok
pub fn check(scope: Rc<RefCell<Scope>>, stmt: &Statement) -> Result<(), (String, Meta)> {
    check_statement(scope, stmt)
}

fn check_statement(scope: Rc<RefCell<Scope>>, stmt: &Statement) -> Result<(), (String, Meta)> {
    match stmt.0 {
        _Statement::Chain(ref fst, ref snd) => {
            let _ = check_statement(scope.clone(), fst)?;
            check_statement(scope, snd)
        }
        _Statement::Let(ref pattern, ref exp) => {
            let _ = check_pattern(scope.clone(), pattern)?;
            check_exp(scope, exp)
        }
        _Statement::Assign(ref id, ref exp) => {
            if scope.borrow().free_or_immutable(id) {
                Err((id.to_string(), stmt.1.clone()))
            } else {
                check_exp(scope, exp)
            }
        }
        _Statement::Exp(ref exp) | _Statement::Throw(ref exp) | _Statement::Return(ref exp) | _Statement::Break(ref exp) => {
            check_exp(scope, exp)
        }
        _Statement::Rec(ref fun_defs) => {
            for (ref id, _) in fun_defs.iter() {
                scope.borrow_mut().binder(id, false);
            }

            for (_, ref fun_lit) in fun_defs.iter() {
                let _ = check_fun_literal(scope.clone(), fun_lit)?;
            }

            Ok(())
        }
    }
}

fn check_exp(scope: Rc<RefCell<Scope>>, exp: &Expression) -> Result<(), (String, Meta)> {
    match exp.0 {
        _Expression::Id(ref id) => {
            if scope.borrow().free(id) {
                Err((id.clone(), exp.1.clone()))
            } else {
                Ok(())
            }
        }

        _Expression::Nil | _Expression::Bool(..) | _Expression::Int(..) | _Expression::Float(..) => Ok(()),

        _Expression::Land(ref lhs, ref rhs) | _Expression::Lor(ref lhs, ref rhs) | _Expression::BinOp(ref lhs, _, ref rhs) => {
            let _ = check_exp(scope.clone(), lhs)?;
            check_exp(scope, rhs)
        }

        _Expression::Not(ref exp) => check_exp(scope, exp),

        _Expression::If(ref cond, ref then, ref else_) => {
            let _ = check_exp(scope.clone(), cond)?;

            match **then {
                None => {}
                Some(ref then) => {
                    let _ = check_statement(Scope::child(scope.clone()), then)?;
                }
            }

            match **else_ {
                None => {}
                Some(ref else_) => {
                    let _ = check_statement(Scope::child(scope), else_)?;
                }
            }

            Ok(())
        }

        _Expression::While(ref cond, ref body) => {
            let _ = check_exp(scope.clone(), cond)?;

            match **body {
                None => {}
                Some(ref body) => {
                    let _ = check_statement(Scope::child(scope.clone()), body)?;
                }
            }

            Ok(())
        }

        _Expression::Try(ref try_body, ref pat, ref catch_body) => {
            match **try_body {
                None => {}
                Some(ref try_body) => {
                    let _ = check_statement(Scope::child(scope.clone()), try_body)?;
                }
            }

            let catch_scope = Scope::child(scope);

            let _ = check_pattern(catch_scope.clone(), pat)?;

            match **catch_body {
                None => {}
                Some(ref catch_body) => {
                    let _ = check_statement(Scope::child(catch_scope.clone()), catch_body)?;
                }
            }

            Ok(())
        }

        _Expression::Case(ref exp, ref bodies) | _Expression::Loop(ref exp, ref bodies) => {
            let _ = check_exp(scope.clone(), exp)?;

            for (ref pats, ref body) in bodies.iter() {
                for pat in pats.0.iter() {
                    let inner_scope = Scope::child(scope.clone());

                    let _ = check_pattern(inner_scope.clone(), pat)?;

                    match pats.1 {
                        None => {}
                        Some(ref guard) => {
                            let _ = check_exp(inner_scope.clone(), guard)?;
                        }
                    }

                    match **body {
                        None => {}
                        Some(ref body) => {
                            let _ = check_statement(inner_scope, body)?;
                        }
                    }
                }
            }

            Ok(())
        }

        _Expression::Application(ref fun, ref args) => {
            let _ = check_exp(scope.clone(), fun)?;

            for ref arg in args.iter() {
                let _ = check_exp(scope.clone(), arg)?;
            }

            Ok(())
        }

        _Expression::Fun(ref fun_lit) => check_fun_literal(scope, fun_lit),
    }
}

fn check_pattern(scope: Rc<RefCell<Scope>>, pat: &Pattern) -> Result<(), (String, Meta)> {
    match pat.0 {
        _Pattern::Id { ref id, mutable } => {
            scope.borrow_mut().binder(id, mutable);
            Ok(())
        }

        _Pattern::Blank | _Pattern::Nil | _Pattern::Bool(..) | _Pattern::Int(..) | _Pattern::Float(..) => Ok(()),
    }
}

fn check_fun_literal(scope: Rc<RefCell<Scope>>, fun: &FunLiteral) -> Result<(), (String, Meta)> {
    for arg in fun.0.iter() {
        let _ = check_pattern(scope.clone(), arg)?;
    }

    match *fun.1 {
        None => Ok(()),
        Some(ref body) => {
            check_statement(scope, body)
        }
    }
}
