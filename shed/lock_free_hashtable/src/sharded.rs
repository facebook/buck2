/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Sharded (almost) lock-free hashtable.

use std::marker;

use crate::raw::LockFreeRawTable;

/// Lock-free hashtable sharded by key hash.
pub struct ShardedLockFreeRawTable<T, const SHARDS: usize> {
    shards: [LockFreeRawTable<T>; SHARDS],
}

impl<T, const SHARDS: usize> ShardedLockFreeRawTable<T, SHARDS> {
    const _ASSERTIONS: () = assert!(SHARDS.is_power_of_two());

    const SHARD_BITS: usize = SHARDS.trailing_zeros() as usize;

    /// Create a new empty hashtable.
    pub const fn new() -> ShardedLockFreeRawTable<T, SHARDS> {
        struct Empty<A>(marker::PhantomData<A>);
        impl<A> Empty<A> {
            #[allow(clippy::declare_interior_mutable_const)]
            const EMPTY: LockFreeRawTable<A> = LockFreeRawTable::new();
        }

        ShardedLockFreeRawTable {
            shards: [Empty::<T>::EMPTY; SHARDS],
        }
    }

    #[inline]
    fn table_for_hash(&self, hash: u64) -> &LockFreeRawTable<T> {
        // `LockFreeRawTable` uses low bits of hash, so we use high bits to select a shard.
        let shard_index = (hash >> (64 - Self::SHARD_BITS)) as usize;
        &self.shards[shard_index]
    }

    /// Find an entry.
    #[inline]
    pub fn lookup(&self, hash: u64, eq: impl Fn(&T) -> bool) -> Option<&T> {
        self.table_for_hash(hash).lookup(hash, eq)
    }

    /// Insert an entry.
    /// If the entry already exists, the existing entry is returned.
    #[inline]
    pub fn insert(
        &self,
        hash: u64,
        value: Box<T>,
        eq: impl Fn(&T, &T) -> bool,
        hash_fn: impl Fn(&T) -> u64,
    ) -> &T {
        self.table_for_hash(hash).insert(hash, value, eq, hash_fn)
    }

    /// Iterate entries in unspecified order.
    #[inline]
    pub fn iter(&self) -> Iter<T, SHARDS> {
        Iter {
            table: self,
            shard: 0,
            iter: self.shards[0].iter(),
        }
    }
}

/// Iterator over all entries in sharded raw table.
pub struct Iter<'a, T, const SHARDS: usize> {
    table: &'a ShardedLockFreeRawTable<T, SHARDS>,
    /// Current iterator shard index.
    shard: usize,
    iter: crate::raw::Iter<'a, T>,
}

impl<'a, T, const SHARDS: usize> Iterator for Iter<'a, T, SHARDS> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(next) = self.iter.next() {
                return Some(next);
            }
            if self.shard >= SHARDS - 1 {
                debug_assert!(self.shard == SHARDS - 1);
                return None;
            }
            self.shard += 1;
            self.iter = self.table.shards[self.shard].iter();
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::hash_map::DefaultHasher;
    use std::collections::HashSet;
    use std::hash::Hash;
    use std::hash::Hasher;

    use crate::sharded::ShardedLockFreeRawTable;

    #[test]
    fn test_shard_bits() {
        assert_eq!(0, ShardedLockFreeRawTable::<u32, 1>::SHARD_BITS);
        assert_eq!(1, ShardedLockFreeRawTable::<u32, 2>::SHARD_BITS);
        assert_eq!(2, ShardedLockFreeRawTable::<u32, 4>::SHARD_BITS);
        assert_eq!(3, ShardedLockFreeRawTable::<u32, 8>::SHARD_BITS);
        assert_eq!(4, ShardedLockFreeRawTable::<u32, 16>::SHARD_BITS);
    }

    fn hash(key: u32) -> u64 {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        hasher.finish()
    }

    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn hash_fn(key: &u32) -> u64 {
        hash(*key)
    }

    #[test]
    fn test_iter() {
        let table = ShardedLockFreeRawTable::<u32, 8>::new();
        let mut expected = Vec::new();
        for i in 0..1000 {
            table.insert(hash(i), Box::new(i), |a, b| a == b, hash_fn);
            expected.push(i);
        }

        let mut collect = HashSet::new();
        for i in table.iter() {
            let inserted = collect.insert(*i);
            assert!(inserted);
        }

        let mut collect = Vec::from_iter(collect);
        collect.sort_unstable();
        assert_eq!(expected, collect);
    }
}
