/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Working with the variants of an `enum`.

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
pub use gazebo_derive::VariantName;

pub trait VariantName {
    fn variant_name(&self) -> &'static str;

    fn variant_name_lowercase(&self) -> &'static str;
}

impl<T> VariantName for Option<T> {
    fn variant_name(&self) -> &'static str {
        match self {
            Self::Some(_) => "Some",
            None => "None",
        }
    }

    fn variant_name_lowercase(&self) -> &'static str {
        match self {
            Self::Some(_) => "some",
            None => "none",
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

    fn variant_name_lowercase(&self) -> &'static str {
        match self {
            Self::Ok(_) => "ok",
            Self::Err(_) => "err",
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
            FooBaz { field: usize },
        }

        let x = MyEnum::Foo;
        assert_eq!(x.variant_name(), "Foo");
        assert_eq!(x.variant_name_lowercase(), "foo");

        let x = MyEnum::Bar(1);
        assert_eq!(x.variant_name(), "Bar");
        assert_eq!(x.variant_name_lowercase(), "bar");

        let x = MyEnum::FooBaz { field: 1 };
        assert_eq!(x.variant_name(), "FooBaz");
        assert_eq!(x.variant_name_lowercase(), "foo_baz");
    }
}
