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

//! Static string value registry.
//!
//! This module provides a mapping from static string value addresses
//! (empty string and single ASCII characters) to their hash-based identifiers.

use std::collections::HashMap;
use std::sync::LazyLock;

use super::get_static_value_id::StaticValueId;
use super::get_static_value_id::hash_empty_string;
use super::get_static_value_id::hash_short_string;
use crate::values::layout::static_string::VALUE_BYTE_STRINGS;
use crate::values::layout::static_string::VALUE_EMPTY_STRING;

/// Static map from untagged static string addresses to their identifiers.
///
/// This map is lazily initialized at runtime and contains mappings for:
/// - Empty string (`""`) -> hash_empty_string()
/// - Single ASCII characters (`"\x00"` to `"\x7F"`) -> hash_short_string(0..128)
pub(super) static STATIC_STRING_ADDR_TO_ID: LazyLock<HashMap<usize, StaticValueId>> =
    LazyLock::new(|| {
        let mut map = HashMap::new();

        // Register empty string
        let empty_addr = VALUE_EMPTY_STRING.unpack().ptr_value().ptr_value_untagged();
        map.insert(empty_addr, hash_empty_string());

        // Register single ASCII character strings (0x00 to 0x7F)
        for (i, repr) in VALUE_BYTE_STRINGS.iter().enumerate() {
            let addr = repr.unpack().ptr_value().ptr_value_untagged();
            map.insert(addr, hash_short_string(i as u8));
        }

        map
    });
