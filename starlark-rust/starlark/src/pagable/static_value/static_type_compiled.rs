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

/// Define a static type-compiled value with automatic pagable registration.
///
/// This macro creates a static `TypeCompiledImplAsStarlarkValue` and registers it
/// for pagable serialization via the inventory crate.
///
/// # Syntax
///
/// ```ignore
/// static_type_compiled!(pub(crate) TYPE_ANY: IsAny, Ty::any());
/// static_type_compiled!(pub(crate) TYPE_NONE: IsNone, Ty::none());
/// ```
///
/// # Generated Code
///
/// The macro expands to:
/// ```ignore
/// pub(crate) static TYPE_ANY: AllocStaticSimple<TypeCompiledImplAsStarlarkValue<IsAny>> =
///     TypeCompiledImplAsStarlarkValue::alloc_static(IsAny, Ty::any());
/// inventory::submit! {
///     StaticFrozenValueEntry::new("TYPE_ANY", || TYPE_ANY.to_frozen_value())
/// }
/// ```
macro_rules! static_type_compiled {
    ($vis:vis $name:ident : $marker:ident, $ty_expr:expr) => {
        $vis static $name: $crate::values::AllocStaticSimple<
            $crate::values::typing::type_compiled::compiled::TypeCompiledImplAsStarlarkValue<
                $crate::values::typing::type_compiled::matchers::$marker,
            >,
        > = $crate::values::typing::type_compiled::compiled::TypeCompiledImplAsStarlarkValue::alloc_static(
            $crate::values::typing::type_compiled::matchers::$marker,
            $ty_expr,
        );

        $crate::__derive_refs::inventory::submit! {
            $crate::__derive_refs::StaticValueEntry::new(
                file!(),
                line!(),
                || $name.to_frozen_value()
            )
        }
    };
}

pub(crate) use static_type_compiled;
