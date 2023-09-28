/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use proc_macro::TokenStream;
use syn::parse_macro_input;
use syn::parse_quote;

fn derive_error_impl(input: TokenStream, krate: syn::Path) -> TokenStream {
    // For now, this is more or less just a stub that forwards to `thiserror`. It doesn't yet do
    // anything interesting.
    let input = parse_macro_input!(input as syn::DeriveInput);

    // We need to generate an invocation of `thiserror::Derive`. This comes with two pieces of
    // complexity, each requiring some cleverness:
    //  1. We can't just generate a `#[derive(thiserror::Error)]`, because we cannot modify the
    //     input! The trick we use is to generate an exact copy of our input, prefixed with the two
    //     attributes below. The `derive` will run first and do all the normal work of the derive
    //     macro, including generating the items we want. `exterminate` will then run next - all it
    //     does is delete the item it's attached to, leaving us with the result we want.
    //  2. `thiserror::Error` generates code which refers to the `thiserror` crate. However, users
    //     of `buck2_error` will want to avoid importing this crate. We could deal with this by
    //     adding a `use buck2_error::reexport::thiserror;` - that in turn has the disadvantage that
    //     the `thiserror` name will show up in the module (and possibly in IDE suggestions). So
    //     instead, we put the entire thing inside a function, to limit the scope of the reexport.
    let name = syn::Ident::new(
        &format!("__macro_generated_by_buck2_error_hidden_{}", input.ident),
        input.ident.span(),
    );
    quote::quote! {
        #[doc(hidden)]
        #[allow(non_snake_case)]
        #[allow(unused)]
        fn #name() {
            use #krate::__for_macro::thiserror;

            #[derive(thiserror::Error)]
            #[#krate::__for_macro::exterminate]
            #input
        }
    }
    .into()
}

#[proc_macro_derive(ErrorForReexport, attributes(backtrace, error, from, source))]
pub fn derive_error_for_reexport(input: TokenStream) -> TokenStream {
    derive_error_impl(input, parse_quote! { ::buck2_error })
}

#[proc_macro_derive(Error, attributes(backtrace, error, from, source))]
pub fn derive_error(input: TokenStream) -> TokenStream {
    derive_error_impl(input, parse_quote! { crate })
}

// Implementation detail of `derive_error`
#[doc(hidden)]
#[proc_macro_attribute]
pub fn exterminate(_attr: TokenStream, _input: TokenStream) -> TokenStream {
    TokenStream::default()
}
