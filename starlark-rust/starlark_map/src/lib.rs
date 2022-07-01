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

//! Ordered map optimized for starlark-rust use cases.

// TODO(nga): fix.
#![allow(clippy::should_implement_trait)]

mod equivalent;
mod hash_value;
mod hashed;
mod hasher;
mod mix_u32;
pub mod small_map;
pub mod small_set;
// TODO(nga): make private.
mod iter;
pub mod vec_map;

pub use equivalent::Equivalent;
pub use hash_value::StarlarkHashValue;
pub use hashed::Hashed;
pub use hasher::StarlarkHasher;
