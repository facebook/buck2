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

/// Register a frozen value type for deserialization.
///
/// Invoke this macro once for each frozen `AValueSimple` Starlark value type
/// that needs to be deserializable. In most cases, the `#[starlark_value]` macro
/// handles registration automatically. Use this macro only when auto-registration
/// doesn't apply.
///
/// Without registration, attempting to deserialize a heap containing that type
/// will fail.
///
/// # Example
///
/// ```ignore
/// register_avalue_simple_frozen!(FrozenDict);
/// ```
#[macro_export]
macro_rules! register_avalue_simple_frozen {
    ($type:ty) => {
        #[allow(unexpected_cfgs)]
        const _: () = {
            #[cfg(feature = "pagable")]
            $crate::__derive_refs::inventory::submit! {
                $crate::__derive_refs::VTableRegistryEntry {
                    deser_type_id: $crate::__derive_refs::DeserTypeId::of::<$type>(),
                    vtable: $crate::__derive_refs::AValueVTable::new::<
                        $crate::__derive_refs::AValueSimple<$type>
                    >(),
                }
            }
        };
    };
}

/// Macro to register a vtable for a special type with a custom AValue implementation.
///
/// This macro is for special types (like `StarlarkStr`, `FrozenTuple`, `ListGen<FrozenListData>`)
/// that use custom AValue implementations instead of `AValueSimple<T>`.
macro_rules! register_special_avalue_frozen {
    ($starlark_value:ty, $avalue:ty) => {
        #[allow(unexpected_cfgs)]
        const _: () = {
            #[cfg(feature = "pagable")]
            inventory::submit! {
                $crate::pagable::vtable_registry::VTableRegistryEntry {
                    deser_type_id: $crate::__derive_refs::DeserTypeId::of::<$starlark_value>(),
                    vtable: $crate::values::layout::vtable::AValueVTable::new::<$avalue>(),
                }
            }
        };
    };
}

pub(crate) use register_special_avalue_frozen;

/// Macro to register a vtable for a generic `TypeMatcher` implementation.
///
/// This macro is for generic `TypeMatcher` types (like `IsListOf<TypeMatcherBox>`,
/// `IsDictOf<TypeMatcherBox, TypeMatcherBox>`) that cannot use the `#[type_matcher]`
/// attribute macro (which only supports non-generic types).
///
/// # Example
///
/// ```ignore
/// register_type_matcher!(IsTupleOf<TypeMatcherBox>);
/// ```
#[macro_export]
macro_rules! register_type_matcher {
    ($matcher:ty) => {
        #[allow(unexpected_cfgs)]
        const _: () = {
            #[cfg(feature = "pagable")]
            $crate::register_avalue_simple_frozen!(
                $crate::values::typing::TypeCompiledImplAsStarlarkValue<
                    $matcher,
                >
            );
        };
    };
}
