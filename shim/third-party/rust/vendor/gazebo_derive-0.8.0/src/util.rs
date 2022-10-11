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
use syn::spanned::Spanned;
use syn::Data;
use syn::DataEnum;
use syn::DataStruct;
use syn::Fields;
use syn::GenericParam;
use syn::Generics;
use syn::Ident;
use syn::Index;
use syn::TypeParamBound;
use syn::Variant;

// Add a bound to every type parameter.
pub fn add_trait_bounds(mut generics: Generics, bound: &TypeParamBound) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(bound.clone());
        }
    }
    generics
}

fn duplicate_struct(data: &DataStruct, duplicate: &TokenStream) -> TokenStream {
    match data.fields {
        Fields::Named(ref fields) => {
            // Self {x: clone(self.x), y: clone(self.y)}
            let xs = fields.named.iter().map(|f| {
                let name = &f.ident;
                quote_spanned! {f.span() =>
                    #name: #duplicate(&self.#name)
                }
            });
            quote! {
                Self { #(#xs, )* }
            }
        }
        Fields::Unnamed(ref fields) => {
            // Self(clone(self.0), clone(self.1))
            let xs = fields.unnamed.iter().enumerate().map(|(i, f)| {
                let index = Index::from(i);
                quote_spanned! {f.span()=>
                    #duplicate(&self.#index)
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

fn duplicate_variant(data: &Variant, duplicate: &TokenStream) -> TokenStream {
    let ctor = &data.ident;
    match data.fields {
        Fields::Named(ref fields) => {
            // Self::Ctor{x,y} => {x: clone(x), y: clone(y)}
            let (pats, es): (Vec<_>, Vec<_>) = fields
                .named
                .iter()
                .map(|f| {
                    let name = &f.ident;
                    (
                        quote_spanned! {f.span() =>
                            #name
                        },
                        quote_spanned! {f.span() =>
                            #name: #duplicate(#name)
                        },
                    )
                })
                .unzip();
            quote! {
                Self::#ctor{ #(#pats, )* } => Self::#ctor{ #(#es, )* }
            }
        }
        Fields::Unnamed(ref fields) => {
            // Self::Ctor(v0,v1) => Self::Ctor(clone(v0), clone(v1))
            let (pats, es): (Vec<_>, Vec<_>) = fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    let var = Ident::new(&format!("v{}", i), f.span());
                    (
                        quote_spanned! {f.span() => #var},
                        quote_spanned! {f.span() => #duplicate(#var)},
                    )
                })
                .unzip();
            quote! {
                Self::#ctor( #(#pats,)* ) => Self::#ctor( #(#es,)* )
            }
        }
        Fields::Unit => {
            // Self::Ctor => Self::Ctor
            quote!(Self::#ctor => Self::#ctor)
        }
    }
}

fn duplicate_enum(data: &DataEnum, duplicate: &TokenStream) -> TokenStream {
    let xs = data
        .variants
        .iter()
        .map(|v| duplicate_variant(v, duplicate));
    quote! {
        match self {
            #(#xs, )*
        }
    }
}

pub fn duplicate_impl(data: &Data, duplicate: &TokenStream) -> TokenStream {
    match data {
        Data::Struct(data) => duplicate_struct(data, duplicate),
        Data::Enum(data) => duplicate_enum(data, duplicate),
        Data::Union(x) => {
            syn::Error::new_spanned(x.union_token, "Can't derive duplication for unions")
                .into_compile_error()
        }
    }
}
