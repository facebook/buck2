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

use std::collections::HashSet;

use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use quote::quote_spanned;
use syn::parse::ParseStream;
use syn::parse_macro_input;
use syn::parse_quote;
use syn::spanned::Spanned;
use syn::Attribute;
use syn::Data;
use syn::DeriveInput;
use syn::GenericArgument;
use syn::GenericParam;
use syn::Generics;
use syn::Lifetime;
use syn::LifetimeDef;
use syn::PathArguments;
use syn::ReturnType;
use syn::Type;
use syn::TypeParamBound;

use crate::for_each_field::for_each_field;

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
    let body = match trace_impl(&input.data, &input.generics) {
        Ok(body) => body,
        Err(e) => {
            return e.to_compile_error().into();
        }
    };
    let gen = quote! {
        unsafe impl #impl_generics starlark::values::Trace<'v> for #name #ty_generics #where_clause {
            #[allow(unused_variables)]
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

fn trace_impl(data: &Data, generics: &Generics) -> syn::Result<TokenStream> {
    let generic_types = generics
        .params
        .iter()
        .filter_map(|x| match x {
            GenericParam::Type(x) => Some(x.ident.to_string()),
            _ => None,
        })
        .collect();

    for_each_field(data, |name, field| {
        if is_ignore(&field.attrs) {
            Ok(quote! {})
        } else if is_static(&field.ty, &generic_types) {
            Ok(quote_spanned! {
                field.span()=>
                starlark::values::Tracer::trace_static(tracer, #name);
            })
        } else {
            Ok(quote_spanned! {
                field.span()=>
                starlark::values::Trace::trace(#name, tracer);
            })
        }
    })
}

/// Return `true` if this type definitely satisfies 'static,
/// which means it has no lifetimes that aren't 'static and no generic parameters
/// (as they might have lifetime bounds on them)
fn is_static(ty: &Type, generics: &HashSet<String>) -> bool {
    let f = |x| is_static(x, generics);

    match ty {
        Type::Array(x) => f(&x.elem),
        Type::BareFn(_) => true,
        Type::Never(_) => true,
        Type::Paren(x) => f(&x.elem),
        Type::Path(x) => {
            x.qself.is_none()
                && x.path.segments.iter().all(|x| {
                    !generics.contains(&x.ident.to_string())
                        && is_static_path_arguments(&x.arguments, generics)
                })
        }
        Type::Ptr(_) => true,
        Type::Reference(x) => f(&x.elem) && is_static_lifetime(x.lifetime.as_ref()),
        Type::Slice(x) => f(&x.elem),
        Type::Tuple(x) => x.elems.iter().all(f),
        _ => false,
    }
}

fn is_static_path_arguments(x: &PathArguments, generics: &HashSet<String>) -> bool {
    let f = |x| is_static(x, generics);

    match x {
        PathArguments::None => true,
        PathArguments::AngleBracketed(x) => x.args.iter().all(|x| match x {
            GenericArgument::Type(x) => f(x),
            GenericArgument::Lifetime(x) => is_static_lifetime(Some(x)),
            _ => false,
        }),
        PathArguments::Parenthesized(x) => match &x.output {
            ReturnType::Default => false, // An inferred type we can't see
            ReturnType::Type(_, ty) => f(ty) && x.inputs.iter().all(f),
        },
    }
}

fn is_static_lifetime(x: Option<&Lifetime>) -> bool {
    match x {
        None => false,
        Some(x) => x.ident == "static",
    }
}
