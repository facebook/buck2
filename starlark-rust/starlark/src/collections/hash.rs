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

use crate::collections::StarlarkHasher;
use crate::small_map::SmallHashValue;
use crate::small_map::SmallHashed;

/// A hash value.
///
/// Contained value must be compatible with a value produced by `StarlarkHasher`
pub type StarlarkHashValue = SmallHashValue<StarlarkHasher>;

/// A key and its hash.
pub type Hashed<K> = SmallHashed<K, StarlarkHasher>;
