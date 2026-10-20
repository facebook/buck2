/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Borrow;
use std::cell::RefCell;
use std::sync::Arc;

use buck2_util::arc_str::ArcSlice;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use hashbrown::HashTable;

/// An interner specific to our AttrCoercionContext used for interning different kinds of attributes.
/// Things specific about this interner:
/// - Requires interned values to be Dupe, so that you can intern both Arc<...> and specific Arc types like ArcStr.
/// - Interner is not static, so it's not required to take up memory for the entire duration of the program.
pub(crate) struct AttrCoercionInterner<T: Dupe + Eq> {
    cache: RefCell<HashTable<(u64, T)>>,
}

impl<T: Dupe + Eq> AttrCoercionInterner<T> {
    pub(crate) fn new() -> Self {
        Self {
            cache: RefCell::new(HashTable::new()),
        }
    }

    #[cfg(test)]
    pub(crate) fn len(&self) -> usize {
        self.cache.borrow().len()
    }

    /// Interns `internable` using a caller-computed bucket hash.
    ///
    /// Equivalent inputs must use the same hash. Collisions are allowed and are resolved with
    /// [`Equiv`].
    pub(crate) fn intern_hashed<S: Equiv<T> + Into<T>>(&self, internable: S, hash: u64) -> T {
        let mut cache = self.cache.borrow_mut();

        if let Some((_h, v)) = cache.find(hash, |(_h, v)| internable.equivalent(v)) {
            return v.dupe();
        }

        let value: T = internable.into();
        cache.insert_unique(hash, (hash, value.dupe()), |(h, _v)| *h);
        value
    }
}

pub(crate) trait Equiv<T> {
    fn equivalent(&self, value: &T) -> bool;
}

impl<T: Eq> Equiv<T> for T {
    fn equivalent(&self, value: &T) -> bool {
        self == value
    }
}

impl Equiv<ArcStr> for &str {
    fn equivalent(&self, value: &ArcStr) -> bool {
        *self == &**value
    }
}

impl<T: Eq> Equiv<Arc<T>> for T {
    fn equivalent(&self, value: &Arc<T>) -> bool {
        self == value.borrow()
    }
}

impl<T: Eq> Equiv<ArcSlice<T>> for Vec<T> {
    fn equivalent(&self, value: &ArcSlice<T>) -> bool {
        self == &**value
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::AttrCoercionInterner;

    #[test]
    fn test_intern() {
        let interner: AttrCoercionInterner<Arc<String>> = AttrCoercionInterner::new();
        let mut interned_strings = Vec::new();
        for i in 0..100000 {
            let interned = interner.intern_hashed(i.to_string(), (i / 2) as u64);
            assert_eq!(i.to_string().as_str(), interned.as_str());
            interned_strings.push(interned);
        }

        for (i, s) in interned_strings.into_iter().enumerate() {
            let interned = interner.intern_hashed(s.as_ref().clone(), (i / 2) as u64);
            assert!(Arc::ptr_eq(&s, &interned));
        }
    }
}
