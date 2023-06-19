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

use std::collections::HashMap;

use proc_macro2::Ident;
use quote::format_ident;
use quote::quote;
use quote::quote_spanned;
use syn::parse_macro_input;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::Attribute;
use syn::DeriveInput;
use syn::Expr;
use syn::ExprLit;
use syn::MetaNameValue;
use syn::Token;

const STARLARK_DOCS_ATTRS: &str = "starlark_docs";

pub(crate) fn derive_docs(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    expand_docs_derive(input)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

fn expand_docs_derive(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let span = input.span();
    let DeriveInput {
        ident: name,
        generics,
        attrs,
        ..
    } = input;

    let parsed_attrs = parse_custom_attributes(attrs)?;

    let use_inventory = if parsed_attrs.contains_key("builtin") {
        quote! {}
    } else {
        quote! {
            use starlark::__derive_refs::inventory as inventory;
        }
    };

    let name_str = name.to_string();
    let custom_attrs: Vec<_> = parsed_attrs
        .into_iter()
        .map(|(k, v)| {
            quote! { (#k, #v)}
        })
        .collect();

    // Complex values take a generic `V`, so we cannot call their impl's __generated_documentation
    // function directly. We need to have a different getter, and to avoid requiring a lifetime,
    // we just use FrozenValue.
    //
    // For now, we just assume if the struct ends in "Gen", it is a starlark_complex_value.
    // It would be simple enough to make this configurable in the future if required.
    let is_complex_value = name.to_string().ends_with("Gen");
    let frozen_name = match is_complex_value {
        true => {
            let frozen = Ident::new(
                &format!(
                    "Frozen{}",
                    name_str.strip_suffix("Gen").unwrap_or(&name_str)
                ),
                name.span(),
            );
            quote_spanned! {span=> #frozen }
        }
        false => {
            if generics.type_params().count() != 0 {
                return Err(syn::Error::new(
                    span,
                    "If a an item name does not end in `Gen`, it must have no type parameters",
                ));
            }

            quote_spanned! {span=> #name }
        }
    };

    let namespace_fn_name = format_ident!("_{}_register_starlark_docs", name_str.to_lowercase());

    Ok(quote_spanned! {span=>
        fn #namespace_fn_name() {
            // `ctor` fails at compile time on wasm32.
            #[cfg(not(target_arch = "wasm32"))]
            {
                #use_inventory
                starlark::__derive_refs::inventory::submit! {
                    starlark::docs::RegisteredDoc {
                        getter: || starlark::docs::RegisteredDoc::for_type::<#frozen_name>(&[#(#custom_attrs),*]),
                    }
                };
            }
        }
    })
}

fn get_attrs(attr: Attribute) -> syn::Result<HashMap<String, String>> {
    let mut found = HashMap::new();
    let args: Punctuated<MetaNameValue, Token![,]> =
        attr.parse_args_with(Punctuated::parse_terminated)?;
    for arg in args {
        match &arg {
            MetaNameValue {
                path,
                value:
                    Expr::Lit(ExprLit {
                        lit: syn::Lit::Str(s),
                        ..
                    }),
                ..
            } => {
                let ident = path.get_ident().unwrap();
                let attr_name = ident.to_string();
                if found.insert(attr_name, s.value()).is_some() {
                    return Err(syn::Error::new(
                        arg.span(),
                        format!("Argument {} was specified twice", ident),
                    ));
                }
            }
            MetaNameValue { path, .. } => {
                return Err(syn::Error::new(
                    arg.span(),
                    format!(
                        "Argument {} must have a string literal value",
                        path.get_ident().unwrap(),
                    ),
                ));
            }
        }
    }
    Ok(found)
}

fn parse_custom_attributes(attrs: Vec<Attribute>) -> syn::Result<HashMap<String, String>> {
    for attr in attrs {
        if attr.path().is_ident(STARLARK_DOCS_ATTRS) {
            return get_attrs(attr);
        }
    }

    Ok(HashMap::new())
}
