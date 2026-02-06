/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Pagable-native trait object serialization using inventory for auto-registration.
//!
//! This module provides a trait object serialization system similar to `typetag`,
//! but using PagableSerialize/PagableBoxDeserialize instead of serde traits.
//!
//! It uses the `inventory` crate for automatic registration of concrete types
//! at program startup, so there's no need for explicit registration calls.
//!
//! # Usage
//!
//! Use the `#[pagable_typetag]` attribute on both the trait definition and
//! each impl block:
//!
//! ```ignore
//! #[pagable::pagable_typetag]
//! trait MyTrait: pagable::typetag::PagableTagged + Send + Sync {
//!     fn do_something(&self);
//! }
//!
//! #[derive(pagable::Pagable)]
//! struct MyImpl { value: i32 }
//!
//! #[pagable::pagable_typetag]
//! impl MyTrait for MyImpl {
//!     fn do_something(&self) { println!("{}", self.value); }
//! }
//! ```
//!
//! # How it works
//!
//! The `#[pagable_typetag]` attribute macro on a trait definition generates:
//! - A trait-specific registration struct
//! - A static registry to collect registered implementations
//! - `inventory::collect!` for the registration struct
//! - `PagableSerialize` impl for `dyn Trait`
//! - `PagableBoxDeserialize` impl for `dyn Trait`
//!
//! The `#[pagable_typetag]` attribute macro on an impl block generates:
//! - `PagableTagged` impl for the concrete type (using the type name as the tag)
//! - `inventory::submit!` to register the type with its tag

use std::collections::HashMap;

use crate::PagableDeserializer;
use crate::PagableSerialize;
use crate::PagableSerializer;

/// Object-safe serialization trait for tagged types.
///
/// This trait is dyn-compatible and used by trait objects to serialize
/// themselves with a type tag.
pub trait PagableTagged: PagableSerialize + Send + Sync {
    /// Get the type tag for this concrete type.
    fn pagable_type_tag(&self) -> &'static str;

    fn serialize_tagged(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        let tag = self.pagable_type_tag();
        serde::Serialize::serialize(&tag, serializer.serde())?;
        self.pagable_serialize(serializer)
    }
}

/// Function pointer type for deserializing a concrete type into a boxed trait object.
pub type DeserializeFn<T> = fn(&mut dyn PagableDeserializer<'_>) -> crate::Result<Box<T>>;

/// Registry created once per tagged trait by `#[pagable_typetag]` to store deserialize functions.
pub struct Registry<T: ?Sized>(pub HashMap<&'static str, DeserializeFn<T>>);

impl<T: ?Sized> Registry<T> {
    /// Deserialize a tagged trait object from a registry of deserializers.
    ///
    /// Used by the `#[pagable_typetag]` macro to implement `PagableBoxDeserialize`
    /// for trait objects.
    pub fn deserialize_tagged(
        &self,
        deserializer: &mut dyn PagableDeserializer<'_>,
    ) -> crate::Result<Box<T>> {
        let tag: String = serde::Deserialize::deserialize(deserializer.serde())?;
        let deserialize_fn = self
            .0
            .get(tag.as_str())
            .ok_or_else(|| crate::__internal::anyhow::anyhow!("Unknown type tag: {}", tag))?;

        (deserialize_fn)(deserializer)
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use std::sync::Arc;

    use pagable::PagableTagged;

    use crate as pagable;
    use crate::Pagable;

    #[crate::pagable_typetag]
    pub trait Named: PagableTagged + Send + Sync + Debug {
        fn name(&self) -> &str;
    }

    #[derive(Pagable, Debug, Eq, PartialEq)]
    pub struct Key {
        pub name: Arc<String>,
    }

    #[crate::pagable_typetag]
    impl Named for Key {
        fn name(&self) -> &str {
            &self.name
        }
    }

    #[test]
    fn test_typetag_roundtrip() -> crate::Result<()> {
        use crate::testing::TestingDeserializer;
        use crate::testing::TestingSerializer;
        use crate::traits::PagableBoxDeserialize;

        let value: Arc<dyn Named> = Arc::new(Key {
            name: Arc::new("test".to_owned()),
        });

        let mut serializer = TestingSerializer::new();
        value.serialize_tagged(&mut serializer)?;
        let bytes = serializer.finish();

        let mut deserializer = TestingDeserializer::new(&bytes);

        let restored: Box<dyn Named> = <dyn Named>::deserialize_box(&mut deserializer)?;

        assert_eq!(restored.name(), "test");
        Ok(())
    }
}
