/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Intern objects in memory.
//!
//! This is similar to [`internment` crate](https://github.com/droundy/internment)
//! but with changes for performance and flexibility.

#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]
// We deliberately make our code stable compatible
#![cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_use_box))]

use std::borrow::Borrow;
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker;
use std::ops::Deref;
use std::ptr;

use gazebo::dupe::Dupe;
use hashbrown::raw::RawIter;
use hashbrown::raw::RawTable;
use parking_lot::const_rwlock;
use parking_lot::RwLock;
use parking_lot::RwLockReadGuard;

pub struct StaticInterner<T: 'static, H = DefaultHasher> {
    tables: [RwLock<RawTable<&'static T>>; 64],
    _marker: marker::PhantomData<H>,
}

/// An interned pointer.
///
/// Equality of this type is a pointer comparison.
/// But note, this works correctly only if `Intern` pointers created
/// from the same instance of `StaticInterner`.
#[derive(Debug, Ord, PartialOrd)]
pub struct Intern<T: 'static> {
    pointer: &'static T,
}

impl<T: 'static> Copy for Intern<T> {}

impl<T: 'static> Clone for Intern<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: 'static> Dupe for Intern<T> {
    fn dupe(&self) -> Self {
        *self
    }
}

impl<T: 'static> Deref for Intern<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.pointer
    }
}

impl<T: 'static> Intern<T> {
    pub fn deref_static(&self) -> &'static T {
        self.pointer
    }
}

impl<T> PartialEq for Intern<T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.pointer, other.pointer)
    }
}

impl<T> Eq for Intern<T> {}

impl<T: Display> Display for Intern<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(self.pointer, f)
    }
}

/// Hash the value before acquiring the lock.
struct Hashed<T, H> {
    hash: u64,
    value: T,
    _marker: marker::PhantomData<H>,
}

impl<T: Hash, H: Hasher + Default> Hashed<T, H> {
    /// Compute the hash.
    fn hash(value: &T) -> u64 {
        let mut hasher = H::default();
        value.hash(&mut hasher);
        hasher.finish()
    }

    fn new(value: T) -> Self {
        let hash = Self::hash(&value);
        Hashed {
            hash,
            value,
            _marker: marker::PhantomData,
        }
    }
}

/// `Self` is "equivalent" to `K` which means `Self` and `K` are logically "equal"
/// and hashes of `Self` and `Q` are equal.
pub trait Equiv<K: ?Sized> {
    /// Compare `Self` and `K`.
    fn equivalent(&self, key: &K) -> bool;
}

impl<Q: ?Sized, K: ?Sized> Equiv<K> for Q
where
    Q: Eq,
    K: Borrow<Q>,
{
    #[inline]
    fn equivalent(&self, key: &K) -> bool {
        *self == *key.borrow()
    }
}

impl<T: 'static, H> StaticInterner<T, H> {
    /// Create a new interner for given type.
    pub const fn new() -> StaticInterner<T, H> {
        StaticInterner {
            tables: [
                // I don't know how to create an array without copy-paste.
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
                const_rwlock(RawTable::new()),
            ],
            _marker: marker::PhantomData,
        }
    }
}

impl<T: 'static, H: Hasher + Default> StaticInterner<T, H> {
    fn table_for_hash(&'static self, hash: usize) -> &'static RwLock<RawTable<&'static T>> {
        // Note hashbrown uses high 8 bits for some secondary hash,
        // and since we are using low 6 bits for partitioning, we are fine.
        &self.tables[hash % self.tables.len()]
    }

    // This takes the values of a Hashed because it's easier than supporting both when the
    // Hashed owns the value and when it just has a reference.
    fn table_get<'a, Q>(table: &'a RawTable<&'static T>, hash: u64, value: &Q) -> Option<&'static T>
    where
        Q: Hash + Equiv<T> + ?Sized,
        T: Eq + Hash,
    {
        table.get(hash, |t| value.equivalent(*t)).copied()
    }
}

impl<T: 'static, H: Hasher + Default> StaticInterner<T, H> {
    /// Allocate a value, or return previously allocated one.
    pub fn intern<Q>(&'static self, value: Q) -> Intern<T>
    where
        Q: Hash + Equiv<T> + Into<T>,
        T: Eq + Hash,
    {
        let hashed = Hashed::<_, H>::new(value);
        let table_for_hash = self.table_for_hash(hashed.hash as usize);
        let guard = table_for_hash.read();
        if let Some(pointer) = Self::table_get(&*guard, hashed.hash, &hashed.value) {
            return Intern { pointer };
        }
        drop(guard);
        self.intern_slow(hashed, table_for_hash)
    }

    #[cold]
    fn intern_slow<Q>(
        &'static self,
        hashed_value: Hashed<Q, H>,
        table_for_hash: &'static RwLock<RawTable<&'static T>>,
    ) -> Intern<T>
    where
        Q: Hash + Equiv<T> + Into<T>,
        T: Eq + Hash,
    {
        let mut guard = table_for_hash.write();
        if let Some(pointer) = Self::table_get(&*guard, hashed_value.hash, &hashed_value.value) {
            return Intern { pointer };
        }
        let pointer = Box::leak(Box::new(hashed_value.value.into()));
        guard.insert(hashed_value.hash, pointer, |t| Hashed::<T, H>::hash(*t));
        Intern { pointer }
    }

    /// Get a value if it has been interned.
    pub fn get<Q>(&'static self, key: &Q) -> Option<Intern<T>>
    where
        Q: Hash + Equiv<T> + ?Sized,
        T: Eq + Hash,
    {
        let hashed = Hashed::<_, H>::new(key);
        let guard = self.table_for_hash(hashed.hash as usize).read();
        Self::table_get(&*guard, hashed.hash, hashed.value).map(|pointer| Intern { pointer })
    }

    /// Iterate over the interned values. The iterator will hold always hold a lock on (a portion of) the interned
    /// data and the user should take care to not make calls to StaticInterner until the returned iterator is dropped.
    pub fn iter(&'static self) -> Iter<'static, T, H> {
        Iter::new(self)
    }
}

pub struct Iter<'a, T: 'static, H: 'static> {
    v: &'static StaticInterner<T, H>,
    table_idx: usize,
    /// The inner iter iterates over the RawTable at `table_idx`. It holds a lock on that table.
    inner: InnerIter<'a, T>,
}

impl<'a, T: 'static, H: 'static> Iter<'a, T, H> {
    fn new(interner: &'static StaticInterner<T, H>) -> Self {
        Self {
            v: interner,
            table_idx: 0,
            inner: InnerIter::new(interner.tables[0].read()),
        }
    }
}

struct InnerIter<'a, T: 'static> {
    // The guard ensures that use of the iter is safe.
    _guard: RwLockReadGuard<'a, RawTable<&'static T>>,
    iter: RawIter<&'static T>,
}

impl<'a, T: 'static> InnerIter<'a, T> {
    fn new(guard: RwLockReadGuard<'a, RawTable<&'static T>>) -> Self {
        let iter = unsafe { guard.iter() };
        Self {
            _guard: guard,
            iter,
        }
    }
}

impl<'a, T: 'static, H> Iterator for Iter<'a, T, H> {
    type Item = Intern<T>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(v) = self.inner.iter.next() {
                return Some(Intern {
                    pointer: unsafe { v.read() },
                });
            }

            self.table_idx += 1;
            if self.table_idx >= self.v.tables.len() {
                return None;
            }

            let guard = self.v.tables[self.table_idx].read();
            self.inner = InnerIter::new(guard);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use crate::StaticInterner;

    static STRING_INTERNER: StaticInterner<String> = StaticInterner::new();

    #[test]
    fn test_intern() {
        assert_eq!(
            STRING_INTERNER.intern("hello".to_owned()),
            STRING_INTERNER.intern("hello".to_owned())
        );
        assert_ne!(
            STRING_INTERNER.intern("hello".to_owned()),
            STRING_INTERNER.intern("world".to_owned())
        );
    }

    // Make sure things work with reallocation.
    #[test]
    fn test_resize() {
        let mut interned_strings = Vec::new();
        for i in 0..100000 {
            let s = i.to_string();
            let interned = STRING_INTERNER.intern(s.clone());
            assert_eq!(&s, &*interned);
            interned_strings.push(interned);
        }

        for s in &interned_strings {
            let interned = STRING_INTERNER.intern(String::clone(s));
            assert_eq!(*s, interned);
        }
    }

    static TEST_GET_INTERNER: StaticInterner<String> = StaticInterner::new();
    #[test]
    fn test_get() {
        let interner = &TEST_GET_INTERNER;
        assert_eq!(interner.get("hello"), None);
        assert_eq!(interner.get(&"hello".to_owned()), None);

        let interned = interner.intern("hello".to_owned());
        assert_eq!(interner.get("hello"), Some(interned));
        assert_eq!(interner.get(&"hello".to_owned()), Some(interned));
        assert_eq!(interner.get("world"), None);
    }

    static TEST_ITER_INTERNER: StaticInterner<&'static str> = StaticInterner::new();
    #[test]
    fn test_iter() {
        let interner = &TEST_ITER_INTERNER;
        assert_eq!(
            interner
                .iter()
                .map(|v| *v)
                .collect::<BTreeSet<&'static str>>(),
            BTreeSet::from([])
        );
        interner.intern("hello");
        interner.intern("cat");
        interner.intern("world");

        assert_eq!(
            interner
                .iter()
                .map(|v| *v)
                .collect::<BTreeSet<&'static str>>(),
            BTreeSet::from(["hello", "cat", "world"])
        );
    }
}
