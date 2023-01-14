/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use quote::quote;
use syn::parse_macro_input;
use syn::parse_quote;
use syn::DeriveInput;
use syn::TypeParamBound;

use crate::util::add_trait_bounds;
use crate::util::duplicate_impl;

pub fn derive_dupe(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_dupe_explicit(input, true)
}

pub fn derive_dupe_(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_dupe_explicit(input, false)
}

fn derive_dupe_explicit(
    input: proc_macro::TokenStream,
    with_traits: bool,
) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    // Add a bound `T: Dupe` to every type parameter T.
    let generics = if with_traits {
        let bound: TypeParamBound = parse_quote!(gazebo::dupe::Dupe);
        add_trait_bounds(input.generics, &bound)
    } else {
        input.generics
    };
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let name = &input.ident;
    let body = duplicate_impl(&input.data, &quote! { gazebo::dupe::Dupe::dupe });
    let gen = quote! {
        impl #impl_generics gazebo::dupe::Dupe for #name #ty_generics #where_clause {
            fn dupe(&self) -> Self {
                #body
            }
        }
    };
    gen.into()
}
