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

use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use pagable::PagableDeserialize;
use pagable::PagableSerialize;

use crate::values::FrozenHeapName;

/// Stable identifier for a `FrozenHeapRef`, derived from the hash of its
/// `FrozenHeapName`. Unlike an auto-incremented counter, this is stable
/// across processes (same heap name → same ID).
///
/// Since we haven't implemented pagable for `FrozenHeapName` yet, we just
/// use the hash of the heap name for now.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PagableSerialize,
    PagableDeserialize
)]
pub(super) struct HeapRefId(u64);

impl HeapRefId {
    #[expect(
        dead_code,
        reason = "used by FrozenFrozenHeap serialization in later diffs"
    )]
    pub(super) fn from_heap_name(name: &FrozenHeapName) -> Self {
        let mut hasher = DefaultHasher::new();
        name.hash(&mut hasher);
        Self(hasher.finish())
    }
}
