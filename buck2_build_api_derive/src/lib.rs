/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

#[allow(unused_extern_crates)] // proc_macro is very special
extern crate proc_macro;

mod provider;

/// Generates the starlark implementation for an internal provider.
///
/// Implements StarlarkValue, ComplexValue, ProviderLike for the provider type and
/// adds a ProviderCallable that's used in starlark as the constructor.
///
/// Return types can be specified directly by adding `#[provider(field_type=SomeIdentifier)]`
/// to a field. The type `SomeIdentifier` must be one that implements
/// `starlark::values::type_repr::StarlarkTypeRepr`
///
/// In starlark, a provider instance will have starlark-accessible attributes for
/// each of its fields.
///
/// In rust, a utility is added for getting the provider from a provider collection like
/// Foo::from_providers(collection).
///
/// You can customize the freeze implementation (for example if you want to do validation
/// at that point) by passing it to the attribute like:
/// `#[internal_provider(creator, freeze=my_freeze)]`
#[proc_macro_attribute]
pub fn internal_provider(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input);
    let attr = syn::parse_macro_input!(attr);
    match provider::define_provider(attr, input) {
        Ok(tokens) => tokens,
        Err(e) => e.to_compile_error().into(),
    }
}
