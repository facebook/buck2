/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::array;

use allocative::Allocative;
use hashbrown::raw::RawTable;
use lock_free_vec::LockFreeVec;
use parking_lot::RwLock;
use parking_lot::RwLockUpgradableReadGuard;
use parking_lot::RwLockWriteGuard;

use crate::impls::key::CowDiceKeyHashed;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key::DiceKeyErasedRef;

/// We bound each shard to only store up to u32 size entry. Together with `SHARDS`s shards, this
/// is capable to ~4 billion keys. After which point, it is probably too large for DICE to
/// store in memory anyways.
#[derive(Allocative, Default)]
struct Shard {
    table: RwLock<RawTable<u32>>,
    #[allocative(skip)] // TODO(nga): do not skip.
    key_by_index: LockFreeVec<DiceKeyErased, KEY_BY_INDEX_BUCKETS>,
}

const KEY_BY_INDEX_BUCKETS: usize =
    lock_free_vec::buckets_for_max_capacity(DiceKeyIndex::MAX_INDEX_IN_SHARD as usize + 1);

impl Shard {
    fn get(
        table: &RwLockUpgradableReadGuard<RawTable<u32>>,
        keys: &LockFreeVec<DiceKeyErased, KEY_BY_INDEX_BUCKETS>,
        key: DiceKeyErasedRef,
        hash: u64,
    ) -> Option<u32> {
        table
            .get(hash, |k| keys.get(*k as usize).unwrap().as_ref() == key)
            .copied()
    }

    fn insert_unique_unchecked(
        table: &mut RwLockWriteGuard<RawTable<u32>>,
        keys: &LockFreeVec<DiceKeyErased, KEY_BY_INDEX_BUCKETS>,
        key: DiceKeyErased,
        hash: u64,
    ) -> u32 {
        assert!(
            keys.len() < DiceKeyIndex::MAX_INDEX_IN_SHARD as usize,
            "too many dice keys"
        );
        let index = keys.len() as u32;
        keys.push_at(index as usize, key).ok().unwrap();
        table.insert(hash, index, |k| keys.get(*k as usize).unwrap().hash());
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
        // `RawTable` uses:
        // * low bits to select bucket
        // * high 8 bits for mask
        // So we should not use these bits as is for shard selection.
        (hash >> 32) as u32 % DiceKeyIndex::SHARDS
    }

    pub(crate) fn index(&self, key: CowDiceKeyHashed) -> DiceKey {
        let hash = key.hash();
        let key = key.into_cow();
        let shard_index = DiceKeyIndex::shard_index_for_hash(hash);
        let key_by_index = &self.shards[shard_index as usize].key_by_index;
        let shard = &self.shards[shard_index as usize];
        let shard = shard.table.upgradable_read();
        let index_in_shard =
            if let Some(index_in_shard) = Shard::get(&shard, key_by_index, key.borrow(), hash) {
                index_in_shard
            } else {
                let mut shard = RwLockUpgradableReadGuard::upgrade(shard);
                Shard::insert_unique_unchecked(&mut shard, key_by_index, key.into_owned(), hash)
            };
        DiceKeyUnpacked {
            shard_index,
            index_in_shard,
        }
        .pack()
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
    use crate::impls::key::CowDiceKeyHashed;
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
            let coin_key = key_index.index(CowDiceKeyHashed::key(key));

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
            let coin_key = key_index.index(CowDiceKeyHashed::key(key));

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
