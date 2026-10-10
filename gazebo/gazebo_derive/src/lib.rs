/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Derivations for the [Gazebo library](https://docs.rs/gazebo/).
//! Usually you would use these derivations via exports from that library.

#[allow(unused_extern_crates)] // proc_macro is very special
extern crate proc_macro;

use syn::DeriveInput;
use syn::parse_macro_input;

mod variant;

/// Derive the `VariantName` trait.
#[proc_macro_derive(VariantName)]
pub fn derive_variant_names(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match variant::derive_variant_names(input) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error().into(),
    }
}
