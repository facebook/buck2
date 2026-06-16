/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! Deduplication and non-inline serialization support for `FrozenHeapRef`.
//!
//! Heaps are serialized in a separate heap table (topologically sorted,
//! dependencies first), and `FrozenHeapRef` references within arena values
//! are just `HeapRefId`s pointing into the table.
//!
//! ## Wire format
//!
//! ```text
//! Heap Table:
//!   [heap_count: u32]
//!   for each heap (topo order, dependencies first):
//!     [HeapRefId: u64]
//!     [FrozenFrozenHeap arena data...]
//!
//! FrozenHeapRef reference (within arena values):
//!   [u8: tag]
//!     0 = None (empty heap ref)
//!     1 = Ref  [HeapRefId: u64]
//! ```

use std::hash::Hasher;

use allocative::Allocative;
use dupe::Dupe;
use pagable::PagableDeserialize;
use pagable::PagableSerialize;
use strong_hash::StrongHash;

use crate::values::FrozenHeapName;

/// Stable identifier for a `FrozenHeapRef`, derived from a strong hash of its
/// `FrozenHeapName`. The hash is computed with blake3 via [`StrongHash`] so
/// the same heap name produces the same ID across processes (unlike
/// `DefaultHasher`, which uses a per-process random seed).
#[derive(
    Debug,
    Clone,
    Copy,
    Dupe,
    PartialEq,
    Eq,
    Hash,
    Allocative,
    PagableSerialize,
    PagableDeserialize
)]
pub struct HeapRefId(u64);

impl HeapRefId {
    pub(crate) fn from_heap_name(name: &FrozenHeapName) -> Self {
        let mut hasher = Blake3StrongHasher::new();
        name.strong_hash(&mut hasher);
        Self(hasher.finish())
    }
}

/// `std::hash::Hasher` adapter over `blake3::Hasher`. Used to drive
/// [`StrongHash`] implementations into a deterministic blake3 digest.
#[derive(Default)]
pub(crate) struct Blake3StrongHasher(blake3::Hasher);

impl Blake3StrongHasher {
    pub(crate) fn new() -> Self {
        Self::default()
    }
}

// Only `write` and `finish` are forwarded; the `write_*` helpers fall back to
// the default impls. blake3 ingests bytes uniformly, so the defaults are fine.
impl Hasher for Blake3StrongHasher {
    fn write(&mut self, bytes: &[u8]) {
        self.0.update(bytes);
    }

    fn finish(&self) -> u64 {
        let bytes = self.0.finalize().as_bytes()[..8]
            .try_into()
            .expect("blake3 digest is at least 8 bytes");
        u64::from_be_bytes(bytes)
    }
}
