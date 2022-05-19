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

use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse::ParseStream, parse_macro_input, parse_quote, spanned::Spanned, Attribute, Data,
    DataEnum, DataStruct, DeriveInput, Field, Fields, GenericParam, Lifetime, LifetimeDef,
    TypeParamBound, Variant,
};

pub fn derive_trace(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);
    let tick_v = GenericParam::Lifetime(LifetimeDef::new(Lifetime::new("'v", Span::call_site())));

    let bound: TypeParamBound = parse_quote!(starlark::values::Trace<'v>);
    let mut has_tick_v = false;
    for param in &mut input.generics.params {
        if let GenericParam::Type(type_param) = param {
            type_param.bounds.push(bound.clone());
        }
        if let GenericParam::Lifetime(t) = param {
            if t.lifetime.ident == "v" {
                has_tick_v = true;
            }
        }
    }
    let mut generics2 = input.generics.clone();

    let (_, ty_generics, where_clause) = input.generics.split_for_impl();
    if !has_tick_v {
        generics2.params.insert(0, tick_v);
    }
    let (impl_generics, _, _) = generics2.split_for_impl();

    let name = &input.ident;
    let body = match trace_impl(&input.data) {
        Ok(body) => body,
        Err(e) => {
            return e.to_compile_error().into();
        }
    };
    let gen = quote! {
        unsafe impl #impl_generics starlark::values::Trace<'v> for #name #ty_generics #where_clause {
            fn trace(&mut self, tracer: &starlark::values::Tracer<'v>) {
                #body
            }
        }
    };
    gen.into()
}

/// Parse attribute `#[trace(unsafe_ignore)]`.
///
/// Currently it fails on any attribute argument other than `unsafe_ignore`.
#[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_impl_dupe))] // The custom_keyword macro
fn is_ignore(attrs: &[Attribute]) -> bool {
    syn::custom_keyword!(unsafe_ignore);

    attrs.iter().any(|a| {
        a.path.is_ident("trace")
            && a.parse_args_with(|input: ParseStream| {
                let ignore = input.parse::<Option<unsafe_ignore>>()?.is_some();
                Ok(ignore)
            })
            .unwrap()
    })
}

fn trace_struct(data: &DataStruct) -> syn::Result<TokenStream> {
    match data.fields {
        Fields::Named(ref fields) => {
            let xs: Vec<_> = fields
                .named
                .iter()
                .filter_map(|f| {
                    if !is_ignore(&f.attrs) {
                        let name = &f.ident;
                        Some(quote_spanned! {f.span() =>
                            starlark::values::Trace::trace(&mut self.#name, tracer);
                        })
                    } else {
                        None
                    }
                })
                .collect();
            Ok(quote! {
                #(#xs)*
            })
        }
        Fields::Unnamed(ref fields) => {
            let xs: Vec<_> = fields
                .unnamed
                .iter()
                .enumerate()
                .filter_map(|(i, f)| {
                    if !is_ignore(&f.attrs) {
                        let i = syn::Index::from(i);
                        Some(quote_spanned! {f.span() => starlark::values::Trace::trace(&mut self.#i, tracer);})
                    } else {
                        None
                    }
                })
                .collect();
            Ok(quote! {
                #(#xs)*
            })
        }
        Fields::Unit => Ok(quote!()),
    }
}

fn trace_enum_variant_field(field: &Field, name: &Ident) -> syn::Result<TokenStream> {
    if is_ignore(&field.attrs) {
        return Ok(quote! {});
    }
    Ok(quote! {
        starlark::values::Trace::trace(#name, tracer);
    })
}

fn trace_enum_variant(variant: &Variant) -> syn::Result<TokenStream> {
    let variant_name = &variant.ident;
    match &variant.fields {
        Fields::Unit => Ok(quote! {
            Self::#variant_name => {}
        }),
        Fields::Named(named) => {
            let names: Vec<_> = named
                .named
                .iter()
                .map(|f| f.ident.as_ref().unwrap())
                .collect();
            let traces: Vec<_> = named
                .named
                .iter()
                .map(|f| trace_enum_variant_field(f, f.ident.as_ref().unwrap()))
                .collect::<syn::Result<_>>()?;
            Ok(quote! {
                Self::#variant_name { #(#names),* } => {
                    #(#traces)*
                }
            })
        }
        Fields::Unnamed(unnamed) => {
            let names: Vec<_> = (0..unnamed.unnamed.len())
                .map(|i| format_ident!("f_{}", i))
                .collect();
            let traces: Vec<_> = unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, f)| trace_enum_variant_field(f, &names[i]))
                .collect::<syn::Result<_>>()?;
            Ok(quote! {
                Self::#variant_name(#(#names),*) => {
                    #(#traces)*
                }
            })
        }
    }
}

fn trace_enum(data: &DataEnum) -> syn::Result<TokenStream> {
    let variants: Vec<_> = data
        .variants
        .iter()
        .map(trace_enum_variant)
        .collect::<syn::Result<_>>()?;
    Ok(quote! {
        match self {
            #(#variants)*
        }
    })
}

fn trace_impl(data: &Data) -> syn::Result<TokenStream> {
    match data {
        Data::Struct(data) => trace_struct(data),
        Data::Enum(data) => trace_enum(data),
        Data::Union(uni) => Err(syn::Error::new_spanned(
            uni.union_token,
            "Can't derive `Trace` for unions",
        )),
    }
}
