/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use proc_macro::TokenStream;

mod derive_allocative;
mod root;

#[proc_macro_derive(Allocative, attributes(allocative))]
pub fn derive_allocative(input: TokenStream) -> TokenStream {
    derive_allocative::derive_allocative(input)
}

#[proc_macro_attribute]
pub fn root(attr: TokenStream, input: TokenStream) -> TokenStream {
    root::root(attr, input)
}
