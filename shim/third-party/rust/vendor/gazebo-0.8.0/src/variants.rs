/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Working with the variants of an `enum`.

/// Generates implementation to unpack the inner data of enum variants. The `unpack_XXX` methods
/// return the unpacked data as a tuple of references of the inner data. The `into_XXX` variants
/// return similarly-structured tuples.
///
/// ```
/// use gazebo::variants::UnpackVariants;
///
/// #[derive(UnpackVariants)]
/// enum MyEnum {
///     Unit,
///     Unnamed(String, usize),
///     Named { x: String, y: usize },
///     OneWithCapInName(bool),
/// }
///
/// let x = MyEnum::Unit;
/// assert_eq!(x.unpack_unit(), Some(()));
/// assert_eq!(x.unpack_unnamed(), None);
/// assert_eq!(x.unpack_named(), None);
/// assert_eq!(x.unpack_one_with_cap_in_name(), None);
///
/// let x = MyEnum::Unnamed("foo".into(), 1);
/// assert_eq!(x.unpack_unnamed(), Some((&"foo".to_owned(), &1usize)));
/// assert_eq!(x.unpack_unit(), None);
/// assert_eq!(x.unpack_named(), None);
/// assert_eq!(x.unpack_one_with_cap_in_name(), None);
///
/// let x = MyEnum::Named {
///     x: "foo".into(),
///     y: 2,
/// };
/// assert_eq!(x.unpack_unit(), None);
/// assert_eq!(x.unpack_unnamed(), None);
/// assert_eq!(x.unpack_named(), Some((&"foo".to_owned(), &2usize)));
/// assert_eq!(x.unpack_one_with_cap_in_name(), None);
///
/// let x = MyEnum::OneWithCapInName(true);
/// assert_eq!(x.unpack_one_with_cap_in_name(), Some(&true));
/// assert_eq!(x.unpack_unit(), None);
/// assert_eq!(x.unpack_unnamed(), None);
/// assert_eq!(x.unpack_named(), None);
///
/// let x = MyEnum::Unnamed("foo".into(), 1);
/// assert_eq!(x.into_unnamed(), Some(("foo".to_owned(), 1usize)));
///
/// let x = MyEnum::Unit;
/// assert_eq!(x.into_unit(), Some(()));
///
/// let x = MyEnum::Named {
///     x: "foo".into(),
///     y: 2,
/// };
/// assert_eq!(x.into_named(), Some(("foo".into(), 2usize)));
///
/// let x = MyEnum::OneWithCapInName(true);
/// assert_eq!(x.into_one_with_cap_in_name(), Some(true));
///
/// assert_eq!(MyEnum::Unit.into_unnamed(), None);
/// assert_eq!(MyEnum::Unit.into_named(), None);
/// assert_eq!(MyEnum::Unit.into_one_with_cap_in_name(), None);
/// ```
pub use gazebo_derive::UnpackVariants;
/// Trait for enums to return the name of the current variant as a `str`. Useful for
/// debugging messages.
///
/// ```
/// use gazebo::variants::VariantName;
///
/// #[derive(VariantName)]
/// enum Foo {
///     Bar,
///     Baz(usize),
///     Qux { i: usize },
/// }
///
/// assert_eq!(Foo::Bar.variant_name(), "Bar");
/// assert_eq!(Foo::Baz(1).variant_name(), "Baz");
/// assert_eq!(Foo::Qux { i: 1 }.variant_name(), "Qux");
/// ```
///
pub use gazebo_derive::VariantName;

pub trait VariantName {
    fn variant_name(&self) -> &'static str;
}

impl<T> VariantName for Option<T> {
    fn variant_name(&self) -> &'static str {
        match self {
            Self::Some(_) => "Some",
            None => "None",
        }
    }
}

impl<T, E> VariantName for Result<T, E> {
    fn variant_name(&self) -> &'static str {
        match self {
            Self::Ok(_) => "Ok",
            Self::Err(_) => "Err",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[allow(unused_imports)] // Not actually unused, this makes testing the derive macro work
    use crate as gazebo;

    #[test]
    fn derive_variant_names() {
        #[allow(unused)] // The fields aren't used, only the variant names
        #[derive(VariantName)]
        enum MyEnum {
            Foo,
            Bar(usize),
            Baz { field: usize },
        }

        let x = MyEnum::Foo;
        assert_eq!(x.variant_name(), "Foo");

        let x = MyEnum::Bar(1);
        assert_eq!(x.variant_name(), "Bar");

        let x = MyEnum::Baz { field: 1 };
        assert_eq!(x.variant_name(), "Baz");
    }
}
