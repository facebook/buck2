/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// This code is adapted from https://github.com/dtolnay/thiserror licensed under Apache-2.0 or MIT.

#![allow(clippy::manual_map)]
#![feature(let_chains)]

mod ast;
mod attr;
mod expand;
mod fmt;
mod generics;
mod prop;
mod valid;

use proc_macro::TokenStream;
use syn::parse_macro_input;
use syn::DeriveInput;

#[proc_macro_derive(Error, attributes(error, source, buck2))]
pub fn derive_error(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand::derive(&input)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
