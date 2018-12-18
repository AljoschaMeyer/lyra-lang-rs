//! Wrappers around foreign types so that they work with the gc.

use im_rc;
use gc::{Trace, Finalize};

/// A garbage-collectable `im::Vector`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Vector<T: Clone>(im_rc::Vector<T>);

impl<T: Trace + Clone> Finalize for Vector<T> {}
unsafe impl<T: Trace + Clone> Trace for Vector<T> {
    custom_trace!(this, {
        for e in this.0.iter() {
            mark(e);
        }
    });
}
