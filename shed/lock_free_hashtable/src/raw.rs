/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! (Almost) lock-free insertion only hashtable.
//!
//! This module provides raw hashtable, which can be used to implement
//! higher-level hashtables.

use std::ptr;
use std::sync::atomic::AtomicPtr;
use std::sync::atomic::Ordering;

use parking_lot::RwLock;

use crate::atomic_value::AtomicValue;
use crate::fixed_cap::FixedCapTable;
pub use crate::fixed_cap::Iter;

struct CurrentTable<T: AtomicValue> {
    /// Previous tables are kept forever because there's no way to know
    /// when they are no longer in use.
    /// We double the capacity every time we resize,
    /// so previous tables allocation overhead is linear in the current capacity.
    _prev: Option<Box<CurrentTable<T>>>,
    /// Table used for insertions and lookups.
    /// When resizing/resized, this table is stored in the `prev` field,
    /// so all the pointers to the table remain valid.
    table: FixedCapTable<T>,
}

/// (Almost) lock-free insertion only hashtable.
pub struct LockFreeRawTable<T: AtomicValue> {
    /// Shared lock for insertion, exclusive lock for resizing.
    /// No lock for lookup.
    write_lock: RwLock<()>,
    /// Current table. Readers can grab the pointer without locking
    /// and pointer remains valid even if the table is resized.
    current: AtomicPtr<CurrentTable<T>>,
}

impl<T: AtomicValue> Drop for LockFreeRawTable<T> {
    fn drop(&mut self) {
        let current = self.current.get_mut();
        if !current.is_null() {
            unsafe {
                let mut current = Box::from_raw(*current);
                // Only the current table owns the entries.
                // Previous tables store subset of the same entries, but we must not drop them.
                current.table.drop_entries();
            };
        }
    }
}

impl<T: AtomicValue> Default for LockFreeRawTable<T> {
    #[inline]
    fn default() -> LockFreeRawTable<T> {
        LockFreeRawTable::new()
    }
}

impl<T: AtomicValue> LockFreeRawTable<T> {
    /// Empty table.
    #[inline]
    pub const fn new() -> LockFreeRawTable<T> {
        LockFreeRawTable {
            write_lock: RwLock::new(()),
            current: AtomicPtr::new(ptr::null_mut()),
        }
    }

    /// Find an entry.
    #[inline]
    pub fn lookup<'a>(&'a self, hash: u64, eq: impl Fn(T::Ref<'_>) -> bool) -> Option<T::Ref<'a>> {
        let current = self.current.load(Ordering::Acquire);

        if current.is_null() {
            return None;
        }

        let current = unsafe { &*current };
        current.table.lookup(hash, eq)
    }

    /// Insert an entry.
    ///
    /// If the entry does not exist, the value is inserted
    /// and a pointer to the inserted entry is returned.
    ///
    /// Otherwise the pointer to existing entry along with the given value is returned.
    pub fn insert<'a>(
        &'a self,
        hash: u64,
        mut value: T,
        eq: impl Fn(T::Ref<'_>, T::Ref<'_>) -> bool,
        hash_fn: impl Fn(T::Ref<'_>) -> u64,
    ) -> (T::Ref<'a>, Option<T>) {
        loop {
            // Acquire shared lock.
            let guard = self.write_lock.read();

            let current = self.current.load(Ordering::Relaxed);

            if current.is_null() {
                // The table is just created. Allocate capacity and start over.
                drop(guard);
                self.resize_if_needed(|v| hash_fn(v));
                continue;
            }

            let current = unsafe { &*current };
            match current.table.insert(hash, value, |a, b| eq(a, b)) {
                Ok((referece, value)) => {
                    drop(guard);
                    // Insert was successful. However, we or other threads
                    // may have exceeded the load factor.
                    // So resize the table if needed to make lookup faster.
                    if current.table.need_resize() {
                        self.resize_if_needed(|v| hash_fn(v));
                    }
                    return (referece, value);
                }
                Err(ret_value) => {
                    drop(guard);
                    // Table is full. Resize the table and try again.
                    self.resize_if_needed(|v| hash_fn(v));
                    value = ret_value;
                }
            }
        }
    }

    #[cold]
    fn resize_if_needed(&self, hash: impl Fn(T::Ref<'_>) -> u64) {
        // Acquire exclusive lock.
        // Readers still read the old table, but new insertions will wait.
        let _guard = self.write_lock.write();

        let current_ptr = self.current.load(Ordering::Relaxed);

        if current_ptr.is_null() {
            let new_table: FixedCapTable<T> = FixedCapTable::with_capacity(16);
            let new_current = Box::new(CurrentTable {
                _prev: None,
                table: new_table,
            });
            self.current
                .store(Box::into_raw(new_current), Ordering::Release);
            return;
        }

        let current = unsafe { &*current_ptr };

        if !current.table.need_resize() {
            return;
        }

        let new_cap = current.table.capacity().checked_mul(2).unwrap();
        let mut new_table: FixedCapTable<T> = FixedCapTable::with_capacity(new_cap);

        for entry_ptr in current.table.iter_ptrs() {
            let entry = unsafe { T::deref(entry_ptr) };
            let hash = hash(entry);
            new_table.insert_unique_unchecked(hash, entry_ptr);
        }

        let new_current = Box::new(CurrentTable {
            _prev: Some(unsafe { Box::from_raw(current_ptr) }),
            table: new_table,
        });

        self.current
            .store(Box::into_raw(new_current), Ordering::Release);
    }

    /// Iterate over all entries.
    pub fn iter(&self) -> Iter<'_, T> {
        let current = self.current.load(Ordering::Acquire);

        if current.is_null() {
            return Iter::empty();
        }

        let current = unsafe { &*current };
        current.table.iter()
    }

    /// Number of entries in the table.
    #[inline]
    pub fn len(&self) -> usize {
        let current = self.current.load(Ordering::Acquire);

        if current.is_null() {
            return 0;
        }

        let current = unsafe { &*current };
        current.table.len()
    }

    /// Number of entries in the table is zero.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[cfg(test)]
mod tests {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hash;
    use std::hash::Hasher;
    use std::num::NonZeroU32;
    use std::ptr;
    use std::ptr::NonNull;

    use crate::atomic_value::RawPtr;
    use crate::raw::LockFreeRawTable;

    fn hash<T: Hash>(key: T) -> u64 {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        hasher.finish()
    }

    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn hash_fn(key: &u32) -> u64 {
        hash(*key)
    }

    #[test]
    fn test_simple() {
        let t = LockFreeRawTable::new();
        let v0 = t.insert(hash(1), Box::new(1), |a, b| a == b, hash_fn).0;
        assert_eq!(&1, v0);
        let v1 = t.insert(hash(1), Box::new(1), |a, b| a == b, hash_fn).0;
        assert_eq!(&1, v1);
        assert!(ptr::eq(v0, v1));

        let v2 = t.lookup(hash(1), |a| a == &1).unwrap();
        assert_eq!(&1, v2);
        assert!(ptr::eq(v0, v2));

        assert_eq!(vec![1], t.iter().cloned().collect::<Vec<_>>());
    }

    #[test]
    fn test_million() {
        let t = LockFreeRawTable::new();
        for i in 0..1000000 {
            t.insert(hash(i), Box::new(i), |a, b| a == b, hash_fn);
        }

        for i in 0..1000000 {
            let v = t.lookup(hash(i), |a| a == &i).unwrap();
            assert_eq!(&i, v);
        }

        assert_eq!(1000000, t.iter().count());
    }

    #[test]
    fn test_u32() {
        let t = LockFreeRawTable::new();
        for i in 1..10000 {
            t.insert(hash(i), NonZeroU32::new(i).unwrap(), |a, b| a == b, hash);
        }

        for i in 1..10000 {
            let v = t.lookup(hash(i), |a| a.get() == i).unwrap();
            assert_eq!(NonZeroU32::new(i).unwrap(), v);
        }
    }

    #[test]
    fn test_raw_pointers() {
        let t = LockFreeRawTable::new();
        let p = NonNull::<u32>::dangling();
        let p = RawPtr(p);
        let r = t.insert(hash(p.0), p, |a, b| a == b, hash).0;
        assert_eq!(p.0, r);
    }
}
