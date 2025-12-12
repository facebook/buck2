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
