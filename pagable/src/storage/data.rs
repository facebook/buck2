/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::num::NonZeroU64;
use std::num::NonZeroU128;

use allocative::Allocative;
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
/// Stored as `[u64; 2]` rather than `u128` to keep 8-byte alignment instead of 16-byte,
/// while remaining bytemuck-compatible. The first half is morally a `NonZeroU64` —
/// `compute()` ensures `self.0[0] != 0` so that [`OptionalDataKey`] can pack itself
/// as `(NonZeroU64, u64)` and inherit the same 8-byte alignment.
///
/// [`PagableArc`]: crate::PagableArc
#[derive(
    Allocative,
    Debug,
    Eq,
    PartialEq,
    Clone,
    Dupe,
    Copy,
    Hash,
    bytemuck::NoUninit
)]
#[repr(transparent)]
pub struct DataKey([u64; 2]);

static_assertions::assert_eq_size!(DataKey, OptionalDataKey);

impl DataKey {
    /// Computes a content-addressable key from serialized data and nested arcs.
    ///
    /// Uses blake3 to hash the arc count, data bytes, and nested arc keys together.
    /// Returns a DataKey with a value guaranteed to be non-zero.
    pub fn compute(arcs: usize, data: &[u8], more_data: &[u8]) -> Self {
        let mut hasher = blake3::Hasher::new();
        hasher.update(&arcs.to_le_bytes());
        hasher.update(data);
        hasher.update(more_data);
        let hash = hasher.finalize();

        // Use the first 128 bits of the blake3 hash
        let hash_bytes = hash.as_bytes();
        let lo = u64::from_le_bytes(hash_bytes[..8].try_into().unwrap());
        let hi = u64::from_le_bytes(hash_bytes[8..16].try_into().unwrap());

        // OptionalDataKey's niche representation requires `lo` to be non-zero. In the
        // astronomically unlikely case of a zero first half, use 1 instead. Collision resistance
        // remains effectively 128 bits.
        if lo == 0 {
            return Self([1, hi]);
        }
        Self([lo, hi])
    }

    pub fn testing_new(v: u128) -> Self {
        assert_ne!(v as u64, 0);
        DataKey(bytemuck::cast(v))
    }

    /// Reconstructs a `DataKey` from the 16-byte form written by
    /// [`bytemuck::bytes_of`] / [`bytemuck::cast_slice`].
    ///
    /// Fails if the bytes encode a zero first half. A `DataKey` always has a non-zero
    /// first half (see [`DataKey::compute`]) so that [`OptionalDataKey`] can use it as a
    /// niche; bytes read back from storage are untrusted, so this is checked here rather
    /// than assumed.
    pub fn from_stored_bytes(bytes: [u8; 16]) -> anyhow::Result<Self> {
        let lo = u64::from_ne_bytes(bytes[..8].try_into().unwrap());
        let hi = u64::from_ne_bytes(bytes[8..].try_into().unwrap());
        if lo == 0 {
            return Err(anyhow::anyhow!(
                "corrupt DataKey: first half must be non-zero"
            ));
        }
        Ok(Self([lo, hi]))
    }

    pub fn get(self) -> u128 {
        self.to_non_zero().get()
    }

    /// Returns the key as a `NonZeroU128`.
    pub fn to_non_zero(self) -> NonZeroU128 {
        NonZeroU128::new(((self.0[1] as u128) << 64) + (self.0[0] as u128))
            .expect("DataKey should never be zero")
    }
}

/// A zero-cost optional representation of [`DataKey`].
///
/// This enum provides the same memory layout as `Option<DataKey>` would have if `DataKey`
/// still used `NonZeroU128` internally, enabling niche optimization. By using an explicit
/// enum instead of `Option<DataKey>`, we can maintain this optimization while allowing
/// `DataKey` to use `u128` for bytemuck compatibility.
#[derive(Debug, Clone, Copy, Hash, Allocative)]
pub enum OptionalDataKey {
    None,
    Some(#[allocative(skip)] NonZeroU64, u64),
}

impl OptionalDataKey {
    pub fn unwrap(&self) -> DataKey {
        match self {
            Self::Some(a, b) => DataKey([a.get(), *b]),
            Self::None => panic!("unwrap called on None"),
        }
    }

    pub fn is_some(&self) -> bool {
        matches!(self, Self::Some(_, _))
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

impl From<DataKey> for OptionalDataKey {
    fn from(key: DataKey) -> Self {
        Self::Some(
            NonZeroU64::new(key.0[0]).expect("DataKey should never be zero"),
            key.0[1],
        )
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
    pub fn compute_key(&self) -> DataKey {
        DataKey::compute(
            self.arcs.len(),
            &self.data,
            bytemuck::cast_slice(&self.arcs),
        )
    }
}
