/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Module providing ref counted operations beyond [`Rc`](std::rc::Rc) and [`Arc`](Arc)
//!

use std::{
    borrow, fmt,
    fmt::{Debug, Display, Formatter},
    ops::Deref,
    sync::{Arc, Weak},
};

use gazebo::prelude::*;

/// Droppable type.
///
/// This is logically `Drop`, but `Drop` is not automatically implemented for all types,
/// but this type is.
trait DropDyn {}

impl<T: ?Sized> DropDyn for T {}

/// Takes a `T` and creates a weak `Rc` to it that is kept alive by `GuardedRcStrongGuard` given by
/// `GuardedRcWeakGuard`. The guards are thread-safe and can be shared across threads, but the `Rc`
/// itself is not.
pub fn guarded_rc<T: Send + 'static>(t: T) -> (GuardedWeakRc<T>, GuardedRcStrongGuard) {
    let ptr = Arc::new(t);
    let weak = Arc::downgrade(&ptr);

    let weak_rc = GuardedWeakRc { inner: weak };

    let guard = GuardedRcStrongGuard::new(ptr);

    (weak_rc, guard)
}

/// A ref counted reference where the value is accessible only in  a single threaded, but its
/// reference counters can be modified in multiple threads. This only holds a Weak reference, which
/// means that the underlying reference may be dropped when there is a ref count of 0.
/// A strong reference can be obtained by `upgrade`.
///
/// valid to send if T is send
/// ```ignore
/// use more_futures::util::guarded_rc::guarded_rc;
///
/// let (x, _) = guarded_rc(());
/// std::thread::spawn(move || x);
/// ```
///
///  
/// ```compile_fail
/// use more_futures::util::guarded_rc::guarded_rc;
///
/// let (x, _) = guarded_rc(());
/// std::thread::spawn( || x.upgrade());
/// ```
pub struct GuardedWeakRc<T: Send + ?Sized> {
    inner: Weak<T>,
}

impl<T: Send> GuardedWeakRc<T> {
    pub fn upgrade(&self) -> Option<GuardedStrongRc<T>> {
        self.inner.upgrade().map(|p| GuardedStrongRc { inner: p })
    }

    /// Create a new guard. This allows attempts to upgrade to a GuardedRcStrongGuard.
    pub fn make_weak_guard(&self) -> GuardedRcWeakGuard
    where
        T: Send + 'static,
    {
        GuardedRcWeakGuard {
            inner: self.inner.dupe(),
        }
    }
}

unsafe impl<T> Send for GuardedWeakRc<T> where T: Send {}
impl<T> !Sync for GuardedWeakRc<T> {}

/// A ref counted reference where the value is accessible only in single threaded context, but its
/// reference counters can be modified in multiple threads. This only holds a strong reference,
/// which adds a value of 1 to the ref count for the life of this value
#[derive(Clone_, Dupe_, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct GuardedStrongRc<T: Send + ?Sized> {
    inner: Arc<T>,
}

impl<T: ?Sized + Debug + Send> Debug for GuardedStrongRc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: ?Sized + Display + Send> Display for GuardedStrongRc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl<T: ?Sized + Send> Deref for GuardedStrongRc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

impl<T: ?Sized + Send> borrow::Borrow<T> for GuardedStrongRc<T> {
    fn borrow(&self) -> &T {
        &**self
    }
}

impl<T: ?Sized + Send> AsRef<T> for GuardedStrongRc<T> {
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T> !Send for GuardedStrongRc<T> {}
impl<T> !Sync for GuardedStrongRc<T> {}

/// A multi-thread safe guard to the `GuardedRc` that provides references to the underlying value
/// to keep it alive, but no access to the actual value.
#[derive(Clone, Dupe)]
pub struct GuardedRcWeakGuard {
    inner: Weak<dyn DropDyn + Send>,
}

impl GuardedRcWeakGuard {
    pub fn upgrade(&self) -> Option<GuardedRcStrongGuard> {
        self.inner
            .upgrade()
            .map(|inner| GuardedRcStrongGuard { inner })
    }
}

/// This does not allow access to the data.
unsafe impl Send for GuardedRcWeakGuard {}
unsafe impl Sync for GuardedRcWeakGuard {}

/// A threadsafe "reference" to the `AtomicRc`, except without being able to access it's values.
///
/// Underlying value is required to be `Send`, but not `Sync`, because the value cannot
/// be accessed using `GuardedRcStrongGuard`, but can be dropped from another thread.
#[derive(Clone, Dupe)]
pub struct GuardedRcStrongGuard {
    inner: Arc<dyn DropDyn + Send>,
}

impl GuardedRcStrongGuard {
    fn new<T: Send + 'static>(ptr: Arc<T>) -> Self {
        Self {
            inner: ptr as Arc<dyn DropDyn + Send>,
        }
    }

    pub fn downgrade(&self) -> GuardedRcWeakGuard {
        GuardedRcWeakGuard {
            inner: Arc::downgrade(&self.inner),
        }
    }
}

/// This does not allow access to the data.
unsafe impl Send for GuardedRcStrongGuard {}
unsafe impl Sync for GuardedRcStrongGuard {}

#[cfg(test)]
pub mod tests {
    use crate::util::guarded_rc::guarded_rc;

    #[test]
    fn weak_rc_lifetime() {
        let (w, g) = guarded_rc("a");
        let wg = g.downgrade();

        assert!(w.upgrade().is_some());
        let g2 = wg.upgrade().expect("should still be alive");

        drop(g);

        if let Some(v) = w.upgrade() {
            drop(g2);

            // v is still held, so should still be around
            assert_eq!("a", *v);

            // can still get new guards since value is still alive
            let _g3 = wg.upgrade().expect("should still be alive");

            drop(v);

            // g3 keeps it alive
            assert!(w.upgrade().is_some());
        } else {
            panic!("should still be alive")
        }

        // every ref guard is now dropped
        assert!(w.upgrade().is_none());
        assert!(wg.upgrade().is_none());
    }
}
