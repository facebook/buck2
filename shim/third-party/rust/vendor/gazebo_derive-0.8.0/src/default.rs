/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use proc_macro2::TokenStream;
use quote::quote;
use quote::quote_spanned;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::Data;
use syn::DataStruct;
use syn::DeriveInput;
use syn::Fields;

pub fn derive_default_(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let name = &input.ident;
    let body = default_impl(&input.data);
    let gen = quote! {
        impl #impl_generics ::std::default::Default for #name #ty_generics #where_clause {
            fn default() -> Self {
                #body
            }
        }
    };
    gen.into()
}

fn default_struct(data: &DataStruct) -> TokenStream {
    match data.fields {
        Fields::Named(ref fields) => {
            // Self {x: Default::default(), y: Default::default()}
            let xs = fields.named.iter().map(|f| {
                let name = &f.ident;
                quote_spanned! {f.span() =>
                    #name: ::std::default::Default::default()
                }
            });
            quote! {
                Self { #(#xs, )* }
            }
        }
        Fields::Unnamed(ref fields) => {
            // Self(Default::default(), Default::default())
            let xs = fields.unnamed.iter().map(|f| {
                quote_spanned! {f.span()=>
                    ::std::default::Default::default()
                }
            });
            quote! {
                Self ( #(#xs, )* )
            }
        }
        Fields::Unit => {
            // Self
            quote!(Self)
        }
    }
}

fn default_impl(data: &Data) -> TokenStream {
    match data {
        Data::Struct(data) => default_struct(data),
        Data::Enum(x) => syn::Error::new_spanned(x.enum_token, "Can't derive Default for enums")
            .into_compile_error(),
        Data::Union(x) => syn::Error::new_spanned(x.union_token, "Can't derive Default for unions")
            .into_compile_error(),
    }
}
