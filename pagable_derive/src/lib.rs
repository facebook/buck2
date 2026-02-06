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

mod derive_pagable;
mod typetag;

#[proc_macro_derive(Pagable, attributes(pagable))]
pub fn derive_pagable(input: TokenStream) -> TokenStream {
    derive_pagable::derive_pagable(input, true, true)
}

#[proc_macro_derive(PagableSerialize, attributes(pagable))]
pub fn derive_pagable_serialize(input: TokenStream) -> TokenStream {
    derive_pagable::derive_pagable(input, true, false)
}

#[proc_macro_derive(PagableDeserialize, attributes(pagable))]
pub fn derive_pagable_deserialize(input: TokenStream) -> TokenStream {
    derive_pagable::derive_pagable(input, false, true)
}

/// Attribute macro for pagable trait object serialization.
///
/// Similar to the `typetag` crate, but for pagable serialization.
///
/// # Usage on traits
///
/// ```ignore
/// #[pagable_typetag]
/// trait MyTrait: pagable::typetag::PagableTagged + Send + Sync {}
/// ```
///
/// This generates:
/// - A static registry for the trait
/// - `PagableSerialize` impl for `dyn MyTrait`
/// - `PagableBoxDeserialize` impl for `dyn MyTrait`
///
/// # Usage on impl blocks
///
/// ```ignore
/// #[derive(Pagable, PagableTagged)]
/// struct MyImpl { value: i32 }
///
/// #[pagable_typetag]
/// impl MyTrait for MyImpl {}
/// ```
///
/// This generates automatic registration via `inventory`.
///
/// **Note:** The type must also derive `PagableTagged` to provide the type tag.
#[proc_macro_attribute]
pub fn pagable_typetag(attr: TokenStream, item: TokenStream) -> TokenStream {
    typetag::pagable_typetag_impl(attr, item)
}
