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

//! VTable registry for Starlark value deserialization.
//!
//! This module provides a mechanism for registering and looking up vtables
//! by their deserialization type identifiers. During deserialization, we need
//! to know which vtable to use for a given type, and this registry provides
//! that mapping.

use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::sync::LazyLock;

use crate::pagable::error::PagableError;
use crate::values::layout::vtable::AValueVTable;

/// Deserialization type identifier for vtable lookup.
///
/// This is a newtype wrapper around `&'static str` that holds the result of
/// `std::any::type_name`. It uniquely identifies a concrete Rust type for
/// deserialization purposes, unlike `StarlarkValue::TYPE` which can be shared
/// (e.g., "function" for EnumType and NativeFunction).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeserTypeId(pub &'static str);

impl DeserTypeId {
    /// Create a `DeserTypeId` for a type.
    #[inline]
    pub const fn of<T: ?Sized>() -> Self {
        DeserTypeId(std::any::type_name::<T>())
    }

    /// Get the underlying type name string.
    #[inline]
    pub const fn as_str(&self) -> &'static str {
        self.0
    }
}

impl Display for DeserTypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.0, f)
    }
}

/// Registry entry for vtable lookup during deserialization.
/// Collected at compile time via the `inventory` crate.
pub struct VTableRegistryEntry {
    /// Deserialization type identifier (from `std::any::type_name`).
    /// Used as the key for vtable lookup during deserialization.
    pub deser_type_id: DeserTypeId,
    /// The vtable for this type.
    pub vtable: &'static AValueVTable,
}

inventory::collect!(VTableRegistryEntry);

/// Lookup table mapping deser_type_id to vtable, built lazily from inventory.
static VTABLE_REGISTRY: LazyLock<HashMap<DeserTypeId, &'static AValueVTable>> =
    LazyLock::new(|| {
        inventory::iter::<VTableRegistryEntry>()
            .map(|e| (e.deser_type_id, e.vtable))
            .collect()
    });

/// Look up a vtable by its deserialization type id.
/// Returns an error if the type is not registered.
#[allow(dead_code)]
pub fn lookup_vtable(deser_type_id: DeserTypeId) -> crate::Result<&'static AValueVTable> {
    VTABLE_REGISTRY.get(&deser_type_id).copied().ok_or_else(|| {
        PagableError::TypeNotRegistered {
            type_id: deser_type_id,
        }
        .into()
    })
}

/// Get a list of all registered type IDs (for debugging/testing).
#[cfg(test)]
pub(crate) fn registered_type_ids() -> Vec<DeserTypeId> {
    VTABLE_REGISTRY.keys().copied().collect()
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use derive_more::Display;
    use starlark_derive::Freeze;
    use starlark_derive::NoSerialize;
    use starlark_derive::Trace;
    use starlark_derive::starlark_value;

    use super::*;
    // Alias crate as starlark so proc macro generated paths work
    use crate as starlark;
    use crate::starlark_complex_value;
    use crate::starlark_simple_value;
    use crate::values::Coerce;
    use crate::values::ProvidesStaticType;
    use crate::values::StarlarkValue;
    use crate::values::ValueLifetimeless;
    use crate::values::ValueLike;

    /// A simple test type to verify vtable registration works for simple values.
    #[derive(Debug, Display, ProvidesStaticType, NoSerialize, Allocative)]
    #[display("TestSimpleType")]
    struct TestSimpleType;

    starlark_simple_value!(TestSimpleType);

    #[starlark_value(type = "TestSimpleType")]
    impl<'v> StarlarkValue<'v> for TestSimpleType {}

    /// A simple complex type to verify vtable registration works for complex values.
    #[derive(
        Debug,
        Display,
        ProvidesStaticType,
        NoSerialize,
        Allocative,
        Clone,
        Trace,
        Freeze,
        Coerce
    )]
    #[display("TestComplex")]
    #[repr(C)]
    struct TestComplexGen<V: ValueLifetimeless> {
        _value: V,
    }

    starlark_complex_value!(TestComplex);

    #[starlark_value(type = "TestComplex")]
    impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for TestComplexGen<V> where Self: ProvidesStaticType<'v>
    {}

    #[test]
    fn test_simple_type_is_registered() {
        // Verify that our simple test type was registered
        let deser_type_id = DeserTypeId::of::<TestSimpleType>();
        let vtable = lookup_vtable(deser_type_id);
        assert!(
            vtable.is_ok(),
            "Expected TestSimpleType to be registered. Available types: {:?}",
            registered_type_ids()
        );
        let vt = vtable.unwrap();
        assert_eq!(vt.type_name, "TestSimpleType");
    }

    #[test]
    fn test_complex_type_frozen_is_registered() {
        // Verify that the frozen variant of complex type was registered
        let type_id = DeserTypeId::of::<FrozenTestComplex>();
        let vtable = lookup_vtable(type_id);
        assert!(
            vtable.is_ok(),
            "Expected FrozenTestComplex to be registered. Available types: {:?}",
            registered_type_ids()
        );
        let vt = vtable.unwrap();
        assert_eq!(vt.type_name, "TestComplex");
    }

    #[test]
    fn test_lookup_nonexistent_type() {
        // Looking up a non-existent type should return an error
        let result = lookup_vtable(DeserTypeId("this_type_does_not_exist_12345"));
        assert!(result.is_err());
        match result {
            Err(err) => match err.kind() {
                crate::ErrorKind::Other(e) => {
                    let pagable_err = e.downcast_ref::<PagableError>().unwrap();
                    assert!(
                        matches!(pagable_err, PagableError::TypeNotRegistered { .. }),
                        "Expected TypeNotRegistered error"
                    );
                }
                _ => panic!("Expected ErrorKind::Other"),
            },
            Ok(_) => panic!("Expected error, got Ok"),
        }
    }

    #[test]
    fn test_starlark_str_is_registered() {
        use crate::values::types::string::str_type::StarlarkStr;
        let type_id = DeserTypeId::of::<StarlarkStr>();
        let vtable = lookup_vtable(type_id);
        assert!(
            vtable.is_ok(),
            "Expected StarlarkStr to be registered. Available types: {:?}",
            registered_type_ids()
        );
        let vt = vtable.unwrap();
        assert!(vt.is_str);
    }

    #[test]
    fn test_frozen_tuple_is_registered() {
        use crate::values::types::tuple::value::FrozenTuple;
        let type_id = DeserTypeId::of::<FrozenTuple>();
        let vtable = lookup_vtable(type_id);
        assert!(
            vtable.is_ok(),
            "Expected FrozenTuple to be registered. Available types: {:?}",
            registered_type_ids()
        );
        let vt = vtable.unwrap();
        assert_eq!(vt.type_name, "tuple");
    }

    #[test]
    fn test_frozen_list_is_registered() {
        use crate::values::list::value::ListGen;
        use crate::values::types::list::value::FrozenListData;
        let type_id = DeserTypeId::of::<ListGen<FrozenListData>>();
        let vtable = lookup_vtable(type_id);
        assert!(
            vtable.is_ok(),
            "Expected ListGen<FrozenListData> to be registered. Available types: {:?}",
            registered_type_ids()
        );
        let vt = vtable.unwrap();
        assert_eq!(vt.type_name, "list");
    }
}
