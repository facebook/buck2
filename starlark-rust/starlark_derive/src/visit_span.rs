/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Variant};

fn derive_variant(variant: &Variant) -> syn::Result<TokenStream> {
    let name = &variant.ident;
    Ok(match &variant.fields {
        Fields::Unit => quote! {
            Self::#name => {}
        },
        Fields::Named(..) => {
            return Err(syn::Error::new_spanned(
                variant,
                "named fields are not supported",
            ));
        }
        Fields::Unnamed(unnamed) => {
            let field_names = (0..unnamed.unnamed.len())
                .map(|i| format_ident!("f{}", i))
                .collect::<Vec<_>>();
            quote! {
                Self::#name(#(#field_names),*) => {
                    #(crate::eval::runtime::visit_span::VisitSpanMut::visit_spans(#field_names, visitor);)*
                }
            }
        }
    })
}

fn derive_body(input: &DeriveInput) -> syn::Result<TokenStream> {
    Ok(match &input.data {
        Data::Enum(e) => {
            let variants = e
                .variants
                .iter()
                .map(derive_variant)
                .collect::<syn::Result<Vec<_>>>()?;
            quote! {
                match self {
                    #(#variants)*
                }
            }
        }
        Data::Struct(s) => match &s.fields {
            Fields::Unit => quote! {},
            Fields::Named(fields) => {
                let field_names = fields
                    .named
                    .iter()
                    .map(|f| f.ident.as_ref().unwrap())
                    .collect::<Vec<_>>();
                quote! {
                    let Self { #(#field_names),* } = self;
                    #(crate::eval::runtime::visit_span::VisitSpanMut::visit_spans(#field_names, visitor);)*
                }
            }
            Fields::Unnamed(fields) => {
                let field_names = (0..fields.unnamed.len())
                    .map(|i| format_ident!("f{}", i))
                    .collect::<Vec<_>>();
                quote! {
                    let Self(#(#field_names),*) = self;
                    #(crate::eval::runtime::visit_span::VisitSpanMut::visit_spans(#field_names, visitor);)*
                }
            }
        },
        Data::Union(u) => {
            return Err(syn::Error::new_spanned(
                u.union_token,
                "VisitSpanMut cannot be derived for unions",
            ));
        }
    })
}

fn derive(input: DeriveInput) -> syn::Result<TokenStream> {
    let (_impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
    let name = &input.ident;
    let body = derive_body(&input)?;

    let impl_generics = if input.generics.params.is_empty() {
        quote! {}
    } else {
        let params = input
            .generics
            .params
            .iter()
            .map(|p| match p {
                syn::GenericParam::Type(t) => {
                    let t = &t.ident;
                    Ok(quote! {
                        #t: crate::eval::runtime::visit_span::VisitSpanMut
                    })
                }
                _ => Err(syn::Error::new_spanned(
                    p,
                    "VisitSpanMut cannot be derived for generics with non-type params",
                )),
            })
            .collect::<syn::Result<Vec<_>>>()?;
        quote! {
            < #(#params,)* >
        }
    };

    Ok(quote! {
        impl #impl_generics crate::eval::runtime::visit_span::VisitSpanMut for #name #type_generics #where_clause {
            fn visit_spans(&mut self, visitor: &mut impl FnMut(&mut crate::eval::runtime::call_stack::FrozenFileSpan)) {
                #body
            }
        }
    })
}

pub(crate) fn derive_visit_span_mut(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive(input) {
        Ok(output) => output.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
