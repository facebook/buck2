/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::array;
use std::num::NonZeroU32;

use allocative::Allocative;
use lock_free_hashtable::raw::LockFreeRawTable;
use lock_free_vec::LockFreeVec;
use parking_lot::Mutex;
use parking_lot::MutexGuard;

use crate::impls::key::CowDiceKeyHashed;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key::DiceKeyErasedRef;
use crate::Key;

const KEY_BY_INDEX_BUCKETS: usize =
    lock_free_vec::buckets_for_max_capacity(DiceKeyIndex::MAX_INDEX_IN_SHARD as usize + 1);

/// We bound each shard to only store up to u32 size entry. Together with `SHARDS`s shards, this
/// is capable to ~4 billion keys. After which point, it is probably too large for DICE to
/// store in memory anyways.
#[derive(Allocative, Default)]
struct Shard {
    table: LockFreeRawTable<NonZeroU32>,
    key_by_index: LockFreeVec<DiceKeyErased, KEY_BY_INDEX_BUCKETS>,
    /// Mutex is used for updates. Lookups do not need to acquire the lock.
    mutex: Mutex<()>,
}

impl Shard {
    fn get(&self, key: DiceKeyErasedRef, hash: u64) -> Option<u32> {
        self.table
            .lookup(hash, |k| {
                self.key_by_index
                    .get(k.get() as usize - 1)
                    .unwrap()
                    .as_ref()
                    == key
            })
            .map(|k| k.get() - 1)
    }

    fn insert_unique_unchecked(
        &self,
        _lock: &MutexGuard<()>,
        key: DiceKeyErased,
        hash: u64,
    ) -> u32 {
        assert!(
            self.key_by_index.len() < DiceKeyIndex::MAX_INDEX_IN_SHARD as usize,
            "too many dice keys"
        );
        let index = self.key_by_index.len() as u32;
        let off_by_one = NonZeroU32::new(index + 1).unwrap();
        self.key_by_index.push_at(index as usize, key).ok().unwrap();
        self.table.insert(
            hash,
            off_by_one,
            |k1, k2| {
                // We could use something like `insert_unique_unchecked`,
                // which currently does not exist in `LockFreeRawTable`.
                k1 == k2
            },
            |k| self.key_by_index.get(k.get() as usize - 1).unwrap().hash(),
        );
        index
    }
}

#[derive(Allocative)]
pub(crate) struct DiceKeyIndex {
    shards: [Shard; DiceKeyIndex::SHARDS as usize],
}

impl Default for DiceKeyIndex {
    fn default() -> DiceKeyIndex {
        DiceKeyIndex {
            shards: array::from_fn(|_| Shard::default()),
        }
    }
}

impl DiceKeyIndex {
    pub(crate) const SHARDS: u32 = 64;
    pub(crate) const MAX_INDEX_IN_SHARD: u32 = u32::MAX / DiceKeyIndex::SHARDS;

    #[inline]
    fn shard_index_for_hash(hash: u64) -> u32 {
        // `LockFreeRawTable` uses low bits to select bucket.
        // So we should not use low bits as is to select shard.
        (hash >> 32) as u32 % DiceKeyIndex::SHARDS
    }

    pub(crate) fn index(&self, key: CowDiceKeyHashed) -> DiceKey {
        let hash = key.hash();
        let key = key.into_cow();
        let shard_index = DiceKeyIndex::shard_index_for_hash(hash);
        let shard = &self.shards[shard_index as usize];

        // First try lookup without locking.
        if let Some(index_in_shard) = shard.get(key.borrow(), hash) {
            return DiceKeyUnpacked {
                shard_index,
                index_in_shard,
            }
            .pack();
        }

        // If not found, lock and try insert.
        let guard = shard.mutex.lock();
        let index_in_shard = if let Some(index_in_shard) = shard.get(key.borrow(), hash) {
            index_in_shard
        } else {
            shard.insert_unique_unchecked(&guard, key.into_owned(), hash)
        };
        DiceKeyUnpacked {
            shard_index,
            index_in_shard,
        }
        .pack()
    }

    pub(crate) fn index_key<K: Key>(&self, key: K) -> DiceKey {
        self.index(CowDiceKeyHashed::key(key))
    }

    pub(crate) fn get(&self, key: DiceKey) -> &DiceKeyErased {
        let unpack = DiceKeyUnpacked::unpack(key);
        self.shards[unpack.shard_index as usize]
            .key_by_index
            .get(unpack.index_in_shard as usize)
            .unwrap()
    }
}

mod introspect {
    use crate::impls::key::DiceKey;
    use crate::impls::key_index::DiceKeyIndex;
    use crate::impls::key_index::DiceKeyUnpacked;
    use crate::introspection::graph::AnyKey;
    use crate::HashMap;

    impl DiceKeyIndex {
        pub(crate) fn introspect(&self) -> HashMap<DiceKey, AnyKey> {
            let mut ret = HashMap::default();

            for (shard_index, shard) in self.shards.iter().enumerate() {
                for (index_in_shard, key) in shard.key_by_index.iter().enumerate() {
                    ret.insert(
                        DiceKeyUnpacked {
                            shard_index: shard_index as u32,
                            index_in_shard: index_in_shard as u32,
                        }
                        .pack(),
                        key.introspect(),
                    );
                }
            }

            ret
        }
    }
}

struct DiceKeyUnpacked {
    shard_index: u32,
    index_in_shard: u32, // actually u26
}

impl DiceKeyUnpacked {
    fn pack(&self) -> DiceKey {
        DiceKey {
            index: self.shard_index + DiceKeyIndex::SHARDS * self.index_in_shard,
        }
    }

    fn unpack(key: DiceKey) -> DiceKeyUnpacked {
        let index = key.index;
        DiceKeyUnpacked {
            shard_index: index % DiceKeyIndex::SHARDS,
            index_in_shard: index / DiceKeyIndex::SHARDS,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cmp;

    use allocative::Allocative;
    use async_trait::async_trait;
    use derive_more::Display;
    use dupe::Dupe;
    use more_futures::cancellation::CancellationContext;

    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::impls::key_index::DiceKeyIndex;
    use crate::impls::key_index::DiceKeyUnpacked;

    #[test]
    fn test_max_index_in_shard() {
        for i in 0..DiceKeyIndex::SHARDS {
            let unpacked = DiceKeyUnpacked {
                shard_index: i,
                index_in_shard: DiceKeyIndex::MAX_INDEX_IN_SHARD,
            };
            let repack = DiceKeyUnpacked::unpack(unpacked.pack());
            assert_eq!(repack.index_in_shard, unpacked.index_in_shard);
            assert_eq!(repack.shard_index, unpacked.shard_index);
        }
    }

    #[test]
    fn test() {
        #[derive(Hash, Clone, Copy, Dupe, Eq, PartialEq, Allocative, Display, Debug)]
        struct TestKey(u64);

        #[async_trait]
        impl Key for TestKey {
            type Value = ();

            async fn compute(
                &self,
                _ctx: &DiceComputations,
                _cancellations: &CancellationContext,
            ) -> Self::Value {
                unimplemented!("not needed")
            }

            fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
                unimplemented!("not needed")
            }
        }

        let key_index = DiceKeyIndex::default();
        let mut max_index = 0;

        let mut seen_shards = [0; DiceKeyIndex::SHARDS as usize];

        let mut i = 0;
        loop {
            let key = TestKey(i);
            let coin_key = key_index.index_key(key);

            assert_eq!(
                i,
                key_index
                    .get(coin_key)
                    .dupe()
                    .downcast::<TestKey>()
                    .unwrap()
                    .0
            );

            seen_shards[DiceKeyUnpacked::unpack(coin_key).shard_index as usize] += 1;
            max_index = cmp::max(max_index, coin_key.index);
            if seen_shards.iter().all(|&x| x != 0) {
                println!(
                    "seen all shards at: {}; min: {}; max: {}; max_index: {}",
                    i,
                    seen_shards.iter().min().unwrap(),
                    seen_shards.iter().max().unwrap(),
                    max_index,
                );
                break;
            }
            i += 1;
        }

        while i < 100000 {
            let key = TestKey(i);
            let coin_key = key_index.index_key(key);

            assert_eq!(
                i,
                key_index
                    .get(coin_key)
                    .dupe()
                    .downcast::<TestKey>()
                    .unwrap()
                    .0
            );

            seen_shards[DiceKeyUnpacked::unpack(coin_key).shard_index as usize] += 1;
            max_index = cmp::max(max_index, coin_key.index);
            i += 1;
        }

        println!(
            "last: {}; min: {}; max: {}; max_index: {}",
            i,
            seen_shards.iter().min().unwrap(),
            seen_shards.iter().max().unwrap(),
            max_index,
        );
    }
}
