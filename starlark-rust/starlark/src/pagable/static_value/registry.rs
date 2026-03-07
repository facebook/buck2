/*
 * Copyright 2026 The Starlark in Rust Authors.
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

//! Registry for static frozen values.
//!
//! Static values are values that exist at fixed memory addresses and are not
//! heap-allocated, such as static strings created by `const_frozen_string!`
//! and singletons like None, True, False, empty collections.
//!
//! These need special handling during serialization/deserialization.
//! We use the inventory crate to collect all registered static values at
//! compile time and build lookup tables at runtime.

use crate::values::FrozenValue;

/// Unified registry entry for a static frozen value.
///
/// Each entry stores the source location where it was registered
/// (used to compute a deterministic hash-based ID at runtime)
/// and a function to obtain the FrozenValue.
pub struct StaticValueEntry {
    /// Source file where this static value was registered (from `file!()`).
    pub file: &'static str,
    /// Source line where this static value was registered (from `line!()`).
    pub line: u32,
    /// Function to get the FrozenValue.
    ///
    /// We use a function pointer instead of storing `FrozenValue` directly
    /// because `inventory::submit!` requires const expressions, and
    /// `FrozenValue` creation involves non-const method calls.
    pub get_value: fn() -> FrozenValue,
}

impl StaticValueEntry {
    /// Create a new registry entry.
    pub const fn new(file: &'static str, line: u32, get_value: fn() -> FrozenValue) -> Self {
        Self {
            file,
            line,
            get_value,
        }
    }
}

inventory::collect!(StaticValueEntry);
