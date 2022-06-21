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

use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use quote::quote_spanned;
use syn::Data;
use syn::DataEnum;
use syn::DataStruct;
use syn::Field;
use syn::Fields;
use syn::Variant;

fn for_each_field_in_struct(
    s: &DataStruct,
    field_handler: impl Fn(&Ident, &Field) -> syn::Result<TokenStream>,
) -> syn::Result<TokenStream> {
    match &s.fields {
        Fields::Unit => Ok(quote! {}),
        Fields::Unnamed(fields) => {
            let field_names: Vec<Ident> = (0..fields.unnamed.len())
                .map(|i| format_ident!("f{}", i))
                .collect();
            let field_handlers = field_names
                .iter()
                .zip(fields.unnamed.iter())
                .map(|(name, field)| field_handler(name, field))
                .collect::<syn::Result<Vec<TokenStream>>>()?;
            Ok(quote_spanned! {
                s.struct_token.span => {
                    let Self(#(#field_names),*) = self;
                    #(#field_handlers)*
                }
            })
        }
        Fields::Named(fields) => {
            let field_names = fields.named.iter().map(|f| &f.ident);
            let field_handlers = fields
                .named
                .iter()
                .map(|f| field_handler(f.ident.as_ref().unwrap(), f))
                .collect::<syn::Result<Vec<TokenStream>>>()?;
            Ok(quote_spanned! {
                s.struct_token.span => {
                    let Self { #(#field_names),* } = self;
                    #(#field_handlers)*
                }
            })
        }
    }
}

fn for_each_field_in_enum_variant(
    v: &Variant,
    field_handler: &impl Fn(&Ident, &Field) -> syn::Result<TokenStream>,
) -> syn::Result<TokenStream> {
    let variant_name = &v.ident;
    match &v.fields {
        Fields::Unit => Ok(quote_spanned! {
            v.ident.span() => Self::#variant_name => {}
        }),
        Fields::Unnamed(fields) => {
            let field_names: Vec<Ident> = (0..fields.unnamed.len())
                .map(|i| format_ident!("f{}", i))
                .collect();
            let field_handlers = field_names
                .iter()
                .zip(fields.unnamed.iter())
                .map(|(name, field)| field_handler(name, field))
                .collect::<syn::Result<Vec<TokenStream>>>()?;
            Ok(quote_spanned! {
                v.ident.span() =>
                Self::#variant_name(#(#field_names),*) => {
                    #(#field_handlers)*
                }
            })
        }
        Fields::Named(fields) => {
            let field_names = fields.named.iter().map(|f| &f.ident);
            let field_handlers = fields
                .named
                .iter()
                .map(|f| field_handler(f.ident.as_ref().unwrap(), f))
                .collect::<syn::Result<Vec<TokenStream>>>()?;
            Ok(quote_spanned! {
                v.ident.span() =>
                Self::#variant_name { #(#field_names),* } => {
                    #(#field_handlers)*
                }
            })
        }
    }
}

fn for_each_field_in_enum(
    e: &DataEnum,
    field_handler: impl Fn(&Ident, &Field) -> syn::Result<TokenStream>,
) -> syn::Result<TokenStream> {
    let variants = e
        .variants
        .iter()
        .map(|v| for_each_field_in_enum_variant(v, &field_handler))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(quote_spanned! {
        e.enum_token.span =>
        match self {
            #(#variants)*
        }
    })
}

/// Generate code for each struct of enum field of the given type.
pub(crate) fn for_each_field(
    data: &Data,
    field_handler: impl Fn(&Ident, &Field) -> syn::Result<TokenStream>,
) -> syn::Result<TokenStream> {
    match data {
        Data::Struct(s) => for_each_field_in_struct(s, field_handler),
        Data::Enum(e) => for_each_field_in_enum(e, field_handler),
        Data::Union(u) => Err(syn::Error::new_spanned(
            u.union_token,
            "Unions are not supported",
        )),
    }
}
