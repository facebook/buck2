/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::num::NonZeroU128;

use dupe::Dupe;

/// A unique identifier for data stored in the paging backend.
///
/// `DataKey` is a 128-bit hash that uniquely identifies serialized data in storage.
/// It is used to retrieve data when a [`PagableArc`] is upgraded
/// after being paged out.
///
/// The 128-bit size provides an extremely low collision probability
/// (< 1e-15 after 820 billion samples).
///
/// The u128 here is morally a NonZeroU128, but this integrates better with bytemuck by using a u128.
#[derive(
    Debug,
    Eq,
    PartialEq,
    Clone,
    Dupe,
    Copy,
    Hash,
    bytemuck::NoUninit,
    bytemuck::AnyBitPattern
)]
#[repr(transparent)]
pub struct DataKey(u128);

static_assertions::assert_eq_size!(DataKey, OptionalDataKey);

impl DataKey {
    /// Computes a content-addressable key from serialized data and nested arcs.
    ///
    /// Uses blake3 to hash the arc count, data bytes, and nested arc keys together.
    /// Returns a DataKey with a value guaranteed to be non-zero.
    pub(crate) fn compute(arcs: usize, data: &[u8], more_data: &[u8]) -> Self {
        let mut hasher = blake3::Hasher::new();
        hasher.update(&arcs.to_le_bytes());
        hasher.update(data);
        hasher.update(more_data);
        let hash = hasher.finalize();

        // Use the first 128 bits of the blake3 hash
        let hash_bytes = hash.as_bytes();
        let value = u128::from_le_bytes(hash_bytes[..16].try_into().unwrap());

        // NonZeroU128 requires a non-zero value. In the astronomically unlikely
        // case of a zero hash, use 1 instead.
        if value == 0 {
            return Self(1);
        }
        Self(value)
    }
}

/// A zero-cost optional representation of [`DataKey`].
///
/// This enum provides the same memory layout as `Option<DataKey>` would have if `DataKey`
/// still used `NonZeroU128` internally, enabling niche optimization. By using an explicit
/// enum instead of `Option<DataKey>`, we can maintain this optimization while allowing
/// `DataKey` to use `u128` for bytemuck compatibility.
#[derive(Debug, Clone, Copy, Hash)]
pub enum OptionalDataKey {
    None,
    Some(NonZeroU128),
}

impl OptionalDataKey {
    pub fn unwrap(&self) -> DataKey {
        match self {
            Self::Some(nz) => DataKey(nz.get()),
            Self::None => panic!("unwrap called on None"),
        }
    }

    pub fn is_some(&self) -> bool {
        matches!(self, Self::Some(_))
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

impl From<DataKey> for OptionalDataKey {
    fn from(key: DataKey) -> Self {
        // SAFETY: DataKey should never be zero (it's morally a NonZeroU128)
        Self::Some(NonZeroU128::new(key.0).expect("DataKey should never be zero"))
    }
}

/// Serialized data retrieved from storage.
///
/// Contains the raw bytes and any nested arc references that need to be resolved
/// during deserialization.
pub struct PagableData {
    pub data: Vec<u8>,
    pub arcs: Vec<DataKey>,
}

impl PagableData {
    /// Computes the content-addressable key for this pagable data.
    ///
    /// The key is computed from the serialized data and nested arc keys.
    pub(crate) fn compute_key(&self) -> DataKey {
        DataKey::compute(
            self.arcs.len(),
            &self.data,
            bytemuck::cast_slice(&self.arcs),
        )
    }
}
