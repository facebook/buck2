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

use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker;
use std::mem;
use std::ops::Deref;
use std::ptr;

use allocative::Allocative;
use allocative::Visitor;
use dupe::Dupe;
use lock_free_hashtable::sharded::ShardedLockFreeRawTable;

pub struct StaticInterner<T: 'static, H = DefaultHasher> {
    table: ShardedLockFreeRawTable<Box<InternedData<T>>, 64>,
    _marker: marker::PhantomData<H>,
}

/// This structure is similar to `Hashed<T>`, but it is not parameterized by hash function.
#[derive(Debug)]
struct InternedData<T: 'static> {
    data: T,
    hash: u64,
}

/// An interned pointer.
///
/// Equality of this type is a pointer comparison.
/// But note, this works correctly only if `Intern` pointers created
/// from the same instance of `StaticInterner`.
#[derive(Debug)]
pub struct Intern<T: 'static> {
    pointer: &'static InternedData<T>,
}

// TODO(nga): derive.
impl<T: Allocative> Allocative for Intern<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        if mem::size_of::<T>() > 0 {
            let visitor = visitor.enter_shared(
                allocative::Key::new("pointer"),
                mem::size_of::<*const T>(),
                &**self as &T as *const T as *const (),
            );
            if let Some(mut visitor) = visitor {
                (**self).visit(&mut visitor);
                visitor.exit();
            }
        }
    }
}

impl<T: 'static> Copy for Intern<T> {}

impl<T: 'static> Clone for Intern<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: 'static> Dupe for Intern<T> {
    #[inline]
    fn dupe(&self) -> Self {
        *self
    }
}

impl<T: 'static> Deref for Intern<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.pointer.data
    }
}

impl<T: 'static> Intern<T> {
    #[inline]
    pub fn deref_static(&self) -> &'static T {
        &self.pointer.data
    }
}

impl<T> Hash for Intern<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // We could hash only the pointer, since we only compare the pointers,
        // but users may expect hashing to be stable between runs.
        self.pointer.hash.hash(state);
    }
}

impl<T> PartialEq for Intern<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.pointer, other.pointer)
    }
}

impl<T> Eq for Intern<T> {}

impl<T: PartialOrd> PartialOrd for Intern<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.pointer.data.partial_cmp(&other.pointer.data)
    }
}

impl<T: Ord> Ord for Intern<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.pointer.data.cmp(&other.pointer.data)
    }
}

impl<T: Display> Display for Intern<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.pointer.data, f)
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

impl<Q: ?Sized, K: ?Sized> Equiv<K> for &Q
where
    Q: Eq,
    K: Borrow<Q>,
{
    #[inline]
    fn equivalent(&self, key: &K) -> bool {
        *self == key.borrow()
    }
}

impl<T: 'static, H> StaticInterner<T, H> {
    /// Create a new interner for given type.
    pub const fn new() -> StaticInterner<T, H> {
        StaticInterner {
            table: ShardedLockFreeRawTable::new(),
            _marker: marker::PhantomData,
        }
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
        if let Some(pointer) = self
            .table
            .lookup(hashed.hash, |t| hashed.value.equivalent(&t.data))
        {
            return Intern { pointer };
        }

        self.intern_slow(hashed)
    }

    #[cold]
    fn intern_slow<Q>(&'static self, hashed_value: Hashed<Q, H>) -> Intern<T>
    where
        Q: Hash + Equiv<T> + Into<T>,
        T: Eq + Hash,
    {
        let pointer = Box::new(InternedData {
            data: hashed_value.value.into(),
            hash: hashed_value.hash,
        });
        let pointer = self.table.insert(
            hashed_value.hash,
            pointer,
            |a, b| a.hash == b.hash && a.data == b.data,
            |t| t.hash,
        );
        Intern { pointer }
    }

    /// Get a value if it has been interned.
    pub fn get<Q>(&'static self, key: Q) -> Option<Intern<T>>
    where
        Q: Hash + Equiv<T>,
        T: Eq + Hash,
    {
        let hashed = Hashed::<_, H>::new(key);
        self.table
            .lookup(hashed.hash, |t| hashed.value.equivalent(&t.data))
            .map(|pointer| Intern { pointer })
    }

    /// Iterate over the interned values.
    #[inline]
    pub fn iter(&'static self) -> Iter<T, H> {
        Iter {
            iter: self.table.iter(),
            _marker: marker::PhantomData,
        }
    }
}

pub struct Iter<T: 'static, H: 'static> {
    iter: lock_free_hashtable::sharded::Iter<'static, Box<InternedData<T>>, 64>,
    _marker: marker::PhantomData<H>,
}

impl<T: 'static, H> Iterator for Iter<T, H> {
    type Item = Intern<T>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|pointer| Intern { pointer })
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
            STRING_INTERNER.intern(&"hello".to_owned()),
            STRING_INTERNER.intern(&"hello".to_owned())
        );
        assert_eq!(
            STRING_INTERNER.intern(&"hello".to_owned()),
            STRING_INTERNER.intern("hello"),
        );
        assert_ne!(
            STRING_INTERNER.intern(&"hello".to_owned()),
            STRING_INTERNER.intern(&"world".to_owned())
        );
    }

    // Make sure things work with reallocation.
    #[test]
    fn test_resize() {
        let mut interned_strings = Vec::new();
        for i in 0..100000 {
            let s = i.to_string();
            let interned = STRING_INTERNER.intern(&s);
            assert_eq!(&s, &*interned);
            interned_strings.push(interned);
        }

        for s in &interned_strings {
            let interned = STRING_INTERNER.intern(&String::clone(s));
            assert_eq!(*s, interned);
        }
    }

    static TEST_GET_INTERNER: StaticInterner<String> = StaticInterner::new();
    #[test]
    fn test_get() {
        let interner = &TEST_GET_INTERNER;
        assert_eq!(interner.get("hello"), None);
        assert_eq!(interner.get(&"hello".to_owned()), None);

        let interned = interner.intern(&"hello".to_owned());
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
