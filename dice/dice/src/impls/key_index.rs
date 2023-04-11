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
use dupe::Dupe;
use hashbrown::raw::RawTable;
use parking_lot::RwLock;
use parking_lot::RwLockUpgradableReadGuard;

use crate::impls::key::CowDiceKey;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key::DiceKeyErasedRef;

/// We bound each shard to only store up to u32 size entry. Together with `SHARDS`s shards, this
/// is capable to ~4 billion keys. After which point, it is probably too large for DICE to
/// store in memory anyways.
#[derive(Allocative, Default)]
struct Shard {
    values: Vec<DiceKeyErased>,
    table: RawTable<u32>,
}

impl Shard {
    fn get(&self, key: DiceKeyErasedRef, hash: u64) -> Option<u32> {
        self.table
            .get(hash, |k| self.values[*k as usize].as_ref() == key)
            .copied()
    }

    fn insert_unique_unchecked(&mut self, key: DiceKeyErased, hash: u64) -> u32 {
        assert!(
            self.values.len() < DiceKeyIndex::MAX_INDEX_IN_SHARD as usize,
            "too many dice keys"
        );
        let index = self.values.len() as u32;
        self.values.push(key);
        self.table
            .insert(hash, index, |k| self.values[*k as usize].hash());
        index
    }
}

#[derive(Allocative)]
pub(crate) struct DiceKeyIndex {
    shards: [RwLock<Shard>; DiceKeyIndex::SHARDS as usize],
}

impl Default for DiceKeyIndex {
    fn default() -> DiceKeyIndex {
        DiceKeyIndex {
            shards: array::from_fn(|_| RwLock::new(Shard::default())),
        }
    }
}

impl DiceKeyIndex {
    pub(crate) const SHARDS: u32 = 64;
    pub(crate) const MAX_INDEX_IN_SHARD: u32 = u32::MAX / DiceKeyIndex::SHARDS;

    pub(crate) fn index(&self, key: CowDiceKey) -> DiceKey {
        let hash = key.borrow().hash();
        let shard_index = (hash as usize % self.shards.len()) as u32; // shard size is bounded to u32
        let shard = &self.shards[shard_index as usize];
        let shard = shard.upgradable_read();
        let index_in_shard = if let Some(index_in_shard) = shard.get(key.borrow(), hash) {
            index_in_shard
        } else {
            let mut shard = RwLockUpgradableReadGuard::upgrade(shard);
            shard.insert_unique_unchecked(key.into_owned(), hash)
        };
        DiceKeyUnpacked {
            shard_index,
            index_in_shard,
        }
        .pack()
    }

    pub(crate) fn get(&self, key: DiceKey) -> DiceKeyErased {
        let unpack = DiceKeyUnpacked::unpack(key);
        let shard = &self.shards[unpack.shard_index as usize];
        let shard = shard.read();
        shard.values[unpack.index_in_shard as usize].dupe()
    }
}

mod introspect {
    use crate::impls::key::DiceKey;
    use crate::impls::key_index::DiceKeyIndex;
    use crate::impls::key_index::DiceKeyUnpacked;
    use crate::introspection::graph::AnyKey;
    use crate::HashMap;

    impl DiceKeyIndex {
        #[allow(unused)] // TODO(bobyf) temporary
        pub(crate) fn introspect(&self) -> HashMap<DiceKey, AnyKey> {
            let mut ret = HashMap::default();

            for (shard_index, shard) in self.shards.iter().enumerate() {
                let shard = shard.read();

                for (index_in_shard, key) in shard.values.iter().enumerate() {
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

    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::impls::key::CowDiceKey;
    use crate::impls::key::DiceKeyErased;
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

            async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
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
            let coin_key = key_index.index(CowDiceKey::Owned(DiceKeyErased::key(key)));

            assert_eq!(i, key_index.get(coin_key).downcast::<TestKey>().unwrap().0);

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
            let coin_key = key_index.index(CowDiceKey::Owned(DiceKeyErased::key(key)));

            assert_eq!(i, key_index.get(coin_key).downcast::<TestKey>().unwrap().0);

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
