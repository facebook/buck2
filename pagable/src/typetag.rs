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
//! - `PagableTypeTag` impl for the concrete type (using the type name as the tag)
//! - `inventory::submit!` to register the type with its tag

use std::collections::HashMap;

use crate::PagableDeserializer;
use crate::PagableSerialize;
use crate::PagableSerializer;

/// Static type tag used to register a concrete type for trait-object deserialization.
pub trait PagableTypeTag: Sized {
    fn pagable_type_tag_static() -> &'static str;
}

/// Object-safe serialization trait for tagged types.
///
/// This trait is dyn-compatible and used by trait objects to serialize
/// themselves with a type tag.
///
/// Notably this trait does **not** inherit from `PagableSerialize`. If it did,
/// Rust would auto-synthesize `impl PagableSerialize for dyn Trait` via the
/// supertrait relation (vtable-dispatched to the concrete type's body-only
/// `pagable_serialize`) and the `#[pagable_typetag]` macro couldn't emit its
/// own `impl PagableSerialize for dyn Trait` that writes `tag + body` (E0371:
/// "the object type automatically implements the trait"). Keeping the
/// relation out lets the macro own the `PagableSerialize` impl for the dyn
/// type so `Arc<dyn Trait>` serialization automatically includes the tag.
pub trait PagableTagged: Send + Sync {
    /// Get the type tag through a trait object, for serialization.
    fn pagable_type_tag(&self) -> &'static str;

    /// Write the body of this value (no tag). Mirrors `PagableSerialize::pagable_serialize`.
    ///
    /// Why this exists (instead of having `serialize_tagged` call
    /// `<Self as PagableSerialize>::pagable_serialize` directly):
    ///
    /// - That call needs `PagableTagged: PagableSerialize`, but we can't have
    ///   that supertrait (see comment on the trait above).
    /// - `where Self: PagableSerialize` → `serialize_tagged` references `Self` in its
    ///   where clause, the whole trait loses `dyn`-compatibility (E0038).
    ///
    /// So `pagable_serialize_body` has to be a genuine vtable-dispatched method.
    /// The blanket implementation below forwards it to `PagableSerialize`.
    fn pagable_serialize_body(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()>;

    /// Write `tag + body`. The `#[pagable_typetag]` macro generates an
    /// `impl PagableSerialize for dyn Trait` that forwards to this method.
    fn serialize_tagged(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        let tag = self.pagable_type_tag();
        serde::Serialize::serialize(&tag, serializer.serde())?;
        self.pagable_serialize_body(serializer)
    }
}

impl<T> PagableTagged for T
where
    T: PagableTypeTag + PagableSerialize + Send + Sync,
{
    fn pagable_type_tag(&self) -> &'static str {
        T::pagable_type_tag_static()
    }

    fn pagable_serialize_body(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        self.pagable_serialize(serializer)
    }
}

/// Marker trait — `PagableTagged` registration for generic wrapper types.
///
/// For a generic `Wrapper<T>`, `#[pagable_tagged(MyDynTrait)]` expands to a
/// `PagableTypeTag` impl gated on `Self: PagableRegisteredFor<dyn
/// MyDynTrait>`:
///
/// ```ignore
/// #[pagable_tagged(MyDynTrait)]
/// struct Wrapper<T: MyInnerTrait>(pub T);
///
/// // Expands to (the blanket impl then provides PagableTagged):
/// impl<T: MyInnerTrait> PagableTypeTag for Wrapper<T>
/// where
///     Self: PagableRegisteredFor<dyn MyDynTrait>,
/// { /* ... */ }
/// ```
///
/// So `Wrapper<X>: PagableTypeTag` and therefore `PagableTagged` exist only when
/// `impl PagableRegisteredFor<dyn MyDynTrait> for Wrapper<X>` does.
///
/// **Don't** implement this trait by hand. Call
/// `register_typetag!(Wrapper<X> as dyn MyDynTrait)` for each pair you use —
/// it emits the `PagableRegisteredFor` impl alongside the inventory
/// registration for pagable.
pub trait PagableRegisteredFor<T: ?Sized> {}

/// Registration entry for a concrete type implementing a trait object.
///
/// Used by both `#[pagable_typetag]` and `register_typetag!`.
pub struct TypetagRegistration<T: ?Sized + 'static> {
    pub tag: fn() -> &'static str,
    pub deserialize: fn(&mut dyn PagableDeserializer<'_>) -> crate::Result<Box<T>>,
}

// SAFETY: TypetagRegistration only contains function pointers (fn types),
// which are inherently Send + Sync.
unsafe impl<T: ?Sized> Send for TypetagRegistration<T> {}
unsafe impl<T: ?Sized> Sync for TypetagRegistration<T> {}

/// A registry built from `TypetagRegistration` entries collected via `inventory`.
pub struct TypetagRegistry<T: ?Sized + 'static> {
    map: HashMap<&'static str, fn(&mut dyn PagableDeserializer<'_>) -> crate::Result<Box<T>>>,
}

impl<T: ?Sized + 'static> TypetagRegistry<T> {
    pub fn from_inventory(iter: impl Iterator<Item = &'static TypetagRegistration<T>>) -> Self {
        let mut map = HashMap::new();
        for reg in iter {
            map.insert((reg.tag)(), reg.deserialize);
        }
        TypetagRegistry { map }
    }

    pub fn deserialize_tagged(
        &self,
        deserializer: &mut dyn PagableDeserializer<'_>,
    ) -> crate::Result<Box<T>> {
        let tag: String = serde::Deserialize::deserialize(deserializer.serde())?;
        let deserialize_fn = self
            .map
            .get(tag.as_str())
            .ok_or_else(|| crate::__internal::anyhow::anyhow!("Unknown type tag: {}", tag))?;
        (deserialize_fn)(deserializer)
    }
}

/// Register a concrete generic instantiation of a wrapper for pagable
/// typetag dispatch. Each call emits the `PagableRegisteredFor<dyn Trait>`
/// impl plus an inventory entry keyed by the concrete type's canonical tag. See
/// [`PagableRegisteredFor`] for the full picture.
///
/// Prerequisite: `Trait` must have `#[pagable_typetag]` applied.
///
/// Examples:
/// ```ignore
/// register_typetag!(Foo<MyInner> as dyn MyTrait);
/// register_typetag!(Foo<A, B> as dyn MyTrait);
/// register_typetag!(Foo<T, T> as dyn MyTrait);
/// ```
#[macro_export]
macro_rules! register_typetag {
    ($concrete:ty as dyn $trait:path) => {
        impl $crate::typetag::PagableRegisteredFor<dyn $trait> for $concrete {}

        $crate::__internal::inventory::submit! {
            <dyn $trait>::__pagable_wrap_registration(
                $crate::typetag::TypetagRegistration {
                    tag: <$concrete as $crate::typetag::PagableTypeTag>::pagable_type_tag_static,
                    deserialize: |deserializer| {
                        let value: $concrete =
                            $crate::PagableDeserialize::pagable_deserialize(deserializer)?;
                        Ok(Box::new(value) as Box<dyn $trait>)
                    },
                }
            )
        }
    };
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use std::sync::Arc;

    use pagable::PagableRegisteredFor;
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

    #[crate::pagable_typetag]
    pub trait SecondaryNamed: PagableTagged + Send + Sync + Debug {
        fn name(&self) -> &str;
    }

    impl SecondaryNamed for Key {
        fn name(&self) -> &str {
            &self.name
        }
    }

    crate::register_typetag!(Key as dyn SecondaryNamed);

    #[derive(Pagable, Debug, Eq, PartialEq)]
    #[crate::pagable_typetag(Named)]
    pub struct Bar {
        pub name: Arc<String>,
    }

    pub trait NamedDyn: PagableTagged + Send + Sync + Debug {
        fn name(&self) -> &str;
    }

    impl NamedDyn for Bar {
        fn name(&self) -> &str {
            &self.name
        }
    }

    impl<T: NamedDyn> Named for T {
        fn name(&self) -> &str {
            NamedDyn::name(self)
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

    #[test]
    fn test_typetag_roundtrip_with_secondary_registration() -> crate::Result<()> {
        use crate::testing::TestingDeserializer;
        use crate::testing::TestingSerializer;
        use crate::traits::PagableBoxDeserialize;

        let value: Arc<dyn SecondaryNamed> = Arc::new(Key {
            name: Arc::new("test".to_owned()),
        });

        assert_eq!(value.pagable_type_tag(), "Key");

        let mut serializer = TestingSerializer::new();
        value.serialize_tagged(&mut serializer)?;
        let bytes = serializer.finish();

        let mut deserializer = TestingDeserializer::new(&bytes);
        let restored: Box<dyn SecondaryNamed> =
            <dyn SecondaryNamed>::deserialize_box(&mut deserializer)?;

        assert_eq!(restored.name(), "test");
        Ok(())
    }

    // --- Generic wrapper tests (like TyCustomFunction<F>) ---
    // Uses register_typetag! + TypetagRegistry instead of #[pagable_typetag]

    /// Trait for the dyn object (like TyCustomDyn)
    #[crate::pagable_typetag]
    pub trait Animal: PagableTagged + Send + Sync + Debug {
        fn species(&self) -> &str;
    }

    /// Generic wrapper (like TyCustomFunction<F>).
    /// The Registered bound on T enforces that only registered inner types can be used.
    #[derive(Debug, Pagable)]
    #[crate::pagable_tagged(Animal)]
    pub struct Wrapper<T: Pagable + Send + Sync + Debug + 'static>(pub T);

    impl<T: Pagable + Send + Sync + Debug + 'static> Animal for Wrapper<T>
    where
        Self: PagableRegisteredFor<dyn Animal>,
    {
        fn species(&self) -> &str {
            "wrapped"
        }
    }

    /// Concrete inner type (like ZipType) — registered
    #[derive(Debug, Pagable, Eq, PartialEq)]
    pub struct Cat;

    // Register Wrapper<Cat> for deserialization as dyn Animal.
    // This generates: impl PagableRegisteredFor<dyn Animal> for Wrapper<Cat> {}
    crate::register_typetag!(Wrapper<Cat> as dyn Animal);

    #[test]
    fn test_register_typetag_generic_roundtrip() -> crate::Result<()> {
        use crate::testing::TestingDeserializer;
        use crate::testing::TestingSerializer;
        use crate::traits::PagableBoxDeserialize;

        let value: Arc<dyn Animal> = Arc::new(Wrapper(Cat));

        let mut serializer = TestingSerializer::new();
        value.serialize_tagged(&mut serializer)?;
        let bytes = serializer.finish();

        let mut deserializer = TestingDeserializer::new(&bytes);
        let restored: Box<dyn Animal> = <dyn Animal>::deserialize_box(&mut deserializer)?;

        assert_eq!(restored.species(), "wrapped");
        Ok(())
    }

    // --- 2-generic wrapper tests ---

    #[derive(Debug, Pagable)]
    #[crate::pagable_tagged(Animal)]
    pub struct Pair<
        A: Pagable + Send + Sync + Debug + 'static,
        B: Pagable + Send + Sync + Debug + 'static,
    >(pub A, pub B);

    impl<A: Pagable + Send + Sync + Debug + 'static, B: Pagable + Send + Sync + Debug + 'static>
        Animal for Pair<A, B>
    where
        Self: PagableRegisteredFor<dyn Animal>,
    {
        fn species(&self) -> &str {
            "paired"
        }
    }

    #[derive(Debug, Pagable, Eq, PartialEq)]
    pub struct Dog;

    // Each register_typetag! emits one impl on the wrapper itself.
    crate::register_typetag!(Pair<Cat, Dog> as dyn Animal);
    crate::register_typetag!(Pair<Cat, Cat> as dyn Animal);

    #[test]
    fn test_register_typetag_pair_distinct_roundtrip() -> crate::Result<()> {
        use crate::testing::TestingDeserializer;
        use crate::testing::TestingSerializer;
        use crate::traits::PagableBoxDeserialize;

        let value: Arc<dyn Animal> = Arc::new(Pair(Cat, Dog));

        let mut serializer = TestingSerializer::new();
        value.serialize_tagged(&mut serializer)?;
        let bytes = serializer.finish();

        let mut deserializer = TestingDeserializer::new(&bytes);
        let restored: Box<dyn Animal> = <dyn Animal>::deserialize_box(&mut deserializer)?;

        assert_eq!(restored.species(), "paired");
        Ok(())
    }

    #[test]
    fn test_register_typetag_pair_same_roundtrip() -> crate::Result<()> {
        use crate::testing::TestingDeserializer;
        use crate::testing::TestingSerializer;
        use crate::traits::PagableBoxDeserialize;

        let value: Arc<dyn Animal> = Arc::new(Pair(Cat, Cat));

        let mut serializer = TestingSerializer::new();
        value.serialize_tagged(&mut serializer)?;
        let bytes = serializer.finish();

        let mut deserializer = TestingDeserializer::new(&bytes);
        let restored: Box<dyn Animal> = <dyn Animal>::deserialize_box(&mut deserializer)?;

        assert_eq!(restored.species(), "paired");
        Ok(())
    }

    #[test]
    fn test_typetag_roundtrip_indirect_impl() -> crate::Result<()> {
        use crate::testing::TestingDeserializer;
        use crate::testing::TestingSerializer;
        use crate::traits::PagableBoxDeserialize;

        let value: Arc<dyn Named> = Arc::new(Bar {
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

    // `#[derive(Pagable)]` on a struct holding `Arc<dyn Trait>` or
    // `Box<dyn Trait>` round-trips.
    #[derive(Debug, Pagable)]
    pub struct AnimalHolder {
        pub animal: Arc<dyn Animal>,
    }

    #[derive(Debug, Pagable)]
    pub struct AnimalHolderBox {
        pub animal: Box<dyn Animal>,
    }

    #[test]
    fn test_pagable_derive_arc_dyn_trait_field_roundtrip() -> crate::Result<()> {
        use crate::PagableDeserialize;
        use crate::PagableSerialize;
        use crate::testing::TestingDeserializer;
        use crate::testing::TestingSerializer;

        // Arc<dyn Animal> round-trip.
        let arc_value = AnimalHolder {
            animal: Arc::new(Wrapper(Cat)),
        };
        let mut serializer = TestingSerializer::new();
        arc_value.pagable_serialize(&mut serializer)?;
        let bytes = serializer.finish();
        let mut deserializer = TestingDeserializer::new(&bytes);
        let restored = AnimalHolder::pagable_deserialize(&mut deserializer)?;
        assert_eq!(restored.animal.species(), "wrapped");

        // Box<dyn Animal> round-trip.
        let box_value = AnimalHolderBox {
            animal: Box::new(Wrapper(Cat)),
        };
        let mut serializer = TestingSerializer::new();
        box_value.pagable_serialize(&mut serializer)?;
        let bytes = serializer.finish();
        let mut deserializer = TestingDeserializer::new(&bytes);
        let restored = AnimalHolderBox::pagable_deserialize(&mut deserializer)?;
        assert_eq!(restored.animal.species(), "wrapped");

        Ok(())
    }
}
