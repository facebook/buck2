/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// TODO(scottcao): use it
#![allow(dead_code)]

use std::cell::RefCell;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker;
use std::sync::Arc;

use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use hashbrown::raw::RawTable;
use twox_hash::XxHash64;

/// An interner specific to our AttrCoercionContext used for interning different kinds of attributes.
/// Things specific about this interner:
/// - Requires interned values to be Dupe, so that you can intern both Arc<...> and specific Arc types like ArcStr.
/// - Interner is not static, so it's not required to take up memory for the entire duration of the program.
pub(crate) struct AttrCoercionInterner<T: Dupe + Hash + Eq, H = XxHash64> {
    /// We use `RawTable` where because `HashMap` API
    /// requires either computing hash twice (for get, then for insert) or
    /// allocating a key to perform a query using `entry` API.
    cache: RefCell<RawTable<(u64, T)>>,
    _marker: marker::PhantomData<H>,
}

impl<T: Dupe + Hash + Eq, H: Hasher + Default> AttrCoercionInterner<T, H> {
    pub(crate) fn new() -> Self {
        Self {
            cache: RefCell::new(RawTable::new()),
            _marker: marker::PhantomData,
        }
    }

    pub(crate) fn intern<S: Internable<T>>(&self, internable: S) -> T {
        fn compute_hash<T: Hash, H: Hasher + Default>(t: T, mut hasher: H) -> u64 {
            t.hash(&mut hasher);
            hasher.finish()
        }

        let hash = compute_hash(&internable, H::default());
        let mut cache = self.cache.borrow_mut();

        if let Some((_h, v)) = cache.get(hash, |(_h, v)| internable.equivalent(v)) {
            return v.dupe();
        }

        let value: T = internable.convert();
        cache.insert(hash, (hash, value.dupe()), |(h, _v)| *h);
        value
    }
}

pub(crate) trait Internable<T>: Hash {
    /// This and the target must produce idencial hashes.
    fn equivalent(&self, value: &T) -> bool;

    fn convert(self) -> T;
}

// TODO(scottcao): add a blanket implementation for anything Dupe

impl Internable<ArcStr> for &str {
    fn equivalent(&self, value: &ArcStr) -> bool {
        *self == &**value
    }

    fn convert(self) -> ArcStr {
        ArcStr::from(self)
    }
}

impl<T: Hash + Eq> Internable<Arc<T>> for T {
    fn equivalent(&self, value: &Arc<T>) -> bool {
        self == &**value
    }

    fn convert(self) -> Arc<T> {
        Arc::from(self)
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
            let interned = interner.intern(i.to_string());
            assert_eq!(i.to_string().as_str(), interned.as_str());
            interned_strings.push(interned);
        }

        for s in interned_strings {
            let interned = interner.intern(s.as_ref().clone());
            assert_eq!(s, interned);
        }
    }
}
