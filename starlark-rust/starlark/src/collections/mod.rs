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

//! Defines [`SmallMap`] and [`SmallSet`] - collections with deterministic iteration and small memory footprint.
//!
//! These structures use vector backed storage if there are only a few elements, and and index
//! for larger collections. The API mirrors standard Rust collections.

pub use crate::collections::hash::Hashed;
pub use crate::collections::hash::StarlarkHashValue;
pub use crate::collections::hasher::StarlarkHasher;
pub use crate::collections::small_map::MHIntoIter;
pub use crate::collections::small_map::MHIter;
pub use crate::collections::small_map::MHIterMut;
pub use crate::collections::small_map::SmallMap;
pub use crate::collections::small_set::SmallSet;
pub use crate::small_map::Equivalent;

pub(crate) mod aligned_padded_str;
pub(crate) mod alloca;
mod hash;
pub(crate) mod hasher;
mod idhasher;
pub mod small_map;
mod small_set;
pub(crate) mod string_pool;
pub(crate) mod symbol_map;
pub(crate) mod vec_map;
