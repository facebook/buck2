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

/// Define a singleton static Starlark value with automatic pagable registration.
///
/// This macro creates a static variable using `AllocStaticSimple` and registers it
/// for pagable serialization via the inventory crate using `file!()` and `line!()`
/// to generate a deterministic hash-based ID.
///
/// # Syntax
///
/// ```ignore
/// static_starlark_value!(pub(crate) VALUE_NONE: NoneType = NoneType);
/// static_starlark_value!(pub(crate) VALUE_EMPTY_TUPLE: FrozenTuple = unsafe { FrozenTuple::new(0) });
/// ```
#[macro_export]
macro_rules! static_starlark_value {
    ($vis:vis $name:ident : $ty:ty = $value:expr) => {
        $vis static $name: $crate::values::AllocStaticSimple<$ty> =
            $crate::values::AllocStaticSimple::alloc($value);

        $crate::__derive_refs::inventory::submit! {
            $crate::__derive_refs::StaticValueEntry::new(
                file!(),
                line!(),
                || $name.to_frozen_value()
            )
        }
    };
}
