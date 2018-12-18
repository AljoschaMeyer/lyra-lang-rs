use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use indexmap::IndexMap;

use super::syntax::*;

/// Returns the set of all lyra toplevel identifiers.
pub fn toplevel() -> HashSet<String> {
    let builtins = vec!["to_string", "typeof", "and", "or"];
    builtins.iter().map(|s| s.to_string()).collect()
}

// State for computing bound/free identifiers.
//
// Also tracks whether there are unused identifiers.
#[derive(Debug)]
pub struct Scope(Rc<_Scope>);

#[derive(Debug)]
pub struct _Scope {
    bound: RefCell<HashSet<String>>,
    unused: RefCell<IndexMap<String, Meta>>,
    parent: Option<Scope>
}

impl Scope {
    fn from_toplevel(toplevel: HashSet<String>) -> Scope {
        Scope(Rc::new(_Scope {
            bound: RefCell::new(toplevel),
            unused: RefCell::new(IndexMap::new()),
            parent: None,
        }))
    }

    fn empty() -> Scope {
        Scope(Rc::new(_Scope {
            bound: RefCell::new(HashSet::new()),
            unused: RefCell::new(IndexMap::new()),
            parent: None,
        }))
    }

    fn child(&self) -> Scope {
        Scope(Rc::new(_Scope {
            bound: RefCell::new(HashSet::new()),
            unused: RefCell::new(IndexMap::new()),
            parent: Some(Scope(self.0.clone())),
        }))
    }

    fn insert(&mut self, id: &Id) {
        self.0.bound.borrow_mut().insert(id.0.clone());
        self.0.unused.borrow_mut().insert(id.0.clone(), id.1.clone());
    }

    /// Returns whether the given id is free in the scope.
    ///
    /// If it is bound, removes it from the set of unused identifiers.
    fn is_free(&self, id: &Id) -> bool {
        if self.0.bound.borrow().contains(&id.0) {
            self.0.unused.borrow_mut().remove(&id.0);
            return false;
        } else {
            match self.0.parent {
                None => return true,
                Some(ref parent) => return parent.is_free(id),
            }
        }
    }
}

/// Checks whether the given expression contains free variables.
/// `toplevel` is a set of names to consider bound in any case.
///
/// Returns `Err` if an id is free. If no id is free, returns `Ok`
/// containing all bound variables that are unused.
pub fn analyze_bindings(exp: &Expression, toplevel: HashSet<String>) -> Result<IndexMap<String, Meta>, Id> {
    let mut scope = Scope::from_toplevel(toplevel);
    let _ = do_analyze(&exp.0, &mut scope)?;
    Ok(Rc::try_unwrap(scope.0).unwrap().unused.into_inner())
}

fn do_analyze(exp: &_Expression, scope: &mut Scope) -> Result<(), Id> {
    match exp {
        _Expression::Nil | _Expression::Args | _Expression::Bool(..) |
        _Expression::Int(..) | _Expression::Char(..) | _Expression::String(..) => Ok(()),
        _Expression::Sequence(ref inners) | _Expression::Set(Set(ref inners)) => {
            for inner in inners.iter() {
                do_analyze(&inner.0, scope)?;
            }
            Ok(())
        }
        _Expression::Map(ref entries) => {
            for entry in entries.iter() {
                do_analyze(&(entry.0).0, scope)?;
                do_analyze(&(entry.1).0, scope)?;
            }
            Ok(())
        }
        _Expression::Land(ref lhs, ref rhs) | _Expression::Lor(ref lhs, ref rhs) => {
            do_analyze(&lhs.0, scope)?;
            do_analyze(&rhs.0, scope)?;
            Ok(())
        }
        _Expression::If(If { ref cond, ref then, ref else_ }) => {
            do_analyze(&cond.0, scope)?;
            do_analyze(&then.0, scope)?;
            if let Some(else__) = else_ {
                do_analyze(&else__.0, scope)?;
            }
            Ok(())
        }
        _Expression::Throw(Throw(ref inner)) => {
            do_analyze(&inner.0, scope)?;
            Ok(())
        }
        _Expression::Try(Try { ref to_try, ref caught, ref catcher }) => {
            do_analyze(&to_try.0, scope)?;
            
        }
        _ => unimplemented!()
    }
}
