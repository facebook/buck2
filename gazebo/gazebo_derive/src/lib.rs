/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Derivations for the [Gazebo library](https://docs.rs/gazebo/).
//! Usually you would use these derivations via exports from that library.

#[allow(unused_extern_crates)] // proc_macro is very special
extern crate proc_macro;

use syn::parse_macro_input;
use syn::DeriveInput;

mod default;
mod variant;

/// Derive the [`Default` trait](Default), but without requiring all type arguments to implement [`Default`](Default).
#[proc_macro_derive(Default_)]
pub fn derive_default_(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    default::derive_default_(input)
}

/// Derive the `VariantName` trait.
#[proc_macro_derive(VariantName)]
pub fn derive_variant_names(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match variant::derive_variant_names(input) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error().into(),
    }
}

// Derive the `Variants` functions.
#[proc_macro_derive(UnpackVariants)]
pub fn derive_variants(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match variant::derive_unpack_variants(input) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error().into(),
    }
}
