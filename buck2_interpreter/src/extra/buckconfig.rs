/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{cell::RefCell, fmt};

use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use hashbrown::raw::RawTable;
use starlark::{
    collections::{BorrowHashed, Hashed},
    environment::Module,
    values::{FrozenStringValue, StringValue},
};

struct BuckConfigEntry {
    section: Hashed<String>,
    key: Hashed<String>,
    value: Option<FrozenStringValue>,
}

/// Version of cell buckconfig optimized for fast query from `read_config` Starlark function.
pub struct LegacyBuckConfigForStarlark<'a> {
    module: &'a Module,
    buckconfig: &'a (dyn LegacyBuckConfigView + 'a),
    /// Hash map by `(section, key)` pair, so we do one table lookup per request.
    /// So we hash the `key` even if the section does not exist,
    /// but this is practically not an issue, because keys usually come with cached hash.
    cache: RefCell<RawTable<BuckConfigEntry>>,
}

impl<'a> fmt::Debug for LegacyBuckConfigForStarlark<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LegacyBuckConfigForStarlark")
            .finish_non_exhaustive()
    }
}

impl<'a> LegacyBuckConfigForStarlark<'a> {
    // `section` or `key` 32 bit hashes are well swizzled,
    // but concatenation of them into 64 bit integer is not.
    // This function tries to fix that.
    fn mix_hashes(a: u32, b: u32) -> u64 {
        fn murmur3_mix64(mut x: u64) -> u64 {
            x ^= x >> 33;
            x = x.wrapping_mul(0xff51afd7ed558ccd);
            x ^= x >> 33;
            x = x.wrapping_mul(0xc4ceb9fe1a85ec53);
            x ^= x >> 33;
            x
        }

        murmur3_mix64(((a as u64) << 32) | (b as u64))
    }

    /// Constructor.
    pub fn new(
        module: &'a Module,
        buckconfig: &'a (dyn LegacyBuckConfigView + 'a),
    ) -> LegacyBuckConfigForStarlark<'a> {
        LegacyBuckConfigForStarlark {
            module,
            buckconfig,
            cache: RefCell::new(RawTable::new()),
        }
    }

    fn get_impl(
        &self,
        section: BorrowHashed<str>,
        key: BorrowHashed<str>,
    ) -> Option<FrozenStringValue> {
        let hash = Self::mix_hashes(section.hash().get(), key.hash().get());
        let mut cache = self.cache.borrow_mut();
        if let Some(e) = cache.get(hash, |e| {
            e.section.key() == section.key() && e.key.as_str() == *key.key()
        }) {
            return e.value;
        }

        let value = self
            .buckconfig
            .get(section.key(), key.key())
            .map(|v| self.module.frozen_heap().alloc_str(&v));

        cache.insert(
            hash,
            BuckConfigEntry {
                section: Hashed::new_unchecked(section.hash(), (*section.key()).to_owned()),
                key: Hashed::new_unchecked(key.hash(), (*key.key()).to_owned()),
                value,
            },
            |e| Self::mix_hashes(e.section.hash().get(), e.key.hash().get()),
        );

        value
    }

    /// Find the buckconfig entry.
    pub fn get(&self, section: StringValue, key: StringValue) -> Option<FrozenStringValue> {
        // Note here we reuse the hashes of `section` and `key`,
        // if `read_config` is called repeatedly with the same constant arguments:
        // `StringValue` caches the hashes.
        self.get_impl(section.get_hashed_str(), key.get_hashed_str())
    }
}
