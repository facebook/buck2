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
use quote::quote;
use quote::quote_spanned;
use syn::parse_macro_input;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::Attribute;
use syn::DeriveInput;
use syn::MetaNameValue;
use syn::Token;

const STARLARK_DOCS_ATTRS: &str = "starlark_docs_attrs";

pub fn derive_docs(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
            quote! { (#k.to_owned(), #v.to_owned())}
        })
        .collect();

    // Complex values take a generic `V`, so we cannot call their impl's __generated_documentation
    // function directly. We need to have a different getter, and to avoid requiring a lifetime,
    // we just use FrozenValue.
    //
    // For now, we just assume if the struct ends in "Gen", it is a starlark_complex_value.
    // It would be simple enough to make this configurable in the future if required.
    let is_complex_value = name.to_string().ends_with("Gen");
    let getter_call = match is_complex_value {
        true => {
            let frozen = Ident::new(
                &format!(
                    "Frozen{}",
                    name_str.strip_suffix("Gen").unwrap_or(&name_str)
                ),
                name.span(),
            );
            quote_spanned! {span=> #frozen::__generated_documentation}
        }
        false => quote_spanned! {span=> #name::__generated_documentation },
    };

    // If we do ConcreteType: Trait<'docs> then we require every instance of ConcreteType
    // to have lifetime 'docs, which breaks for StarlarkStr at least.
    let (constraint_lifetime, constraint) = if generics.params.is_empty() {
        (quote! {}, quote! {})
    } else {
        (
            quote! {<'__docs>},
            quote! { Self: starlark::values::StarlarkValue<'__docs>},
        )
    };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    Ok(quote_spanned! {span=>
        impl #impl_generics #name #ty_generics #where_clause  {
            // Use 'docs here instead of 'v because someone might have 'v in their generics'
            // constraints, and we'd end up with duplicate lifetime definition errors.
            #[doc(hidden)]
            pub fn __generated_documentation #constraint_lifetime () -> Option<starlark::values::docs::Doc>
            where #constraint {
                #use_inventory
                starlark::__derive_refs::inventory::submit! {
                    starlark::values::docs::RegisteredDoc {
                        getter: #getter_call
                    }
                };

                let name = <Self as starlark::values::StarlarkValue>::get_type_value_static().as_str().to_owned();
                let id = starlark::values::docs::Identifier {
                    name,
                    location: None,
                };
                let item = <Self as starlark::values::StarlarkValue>::get_methods()?.documentation();
                let custom_attrs = std::collections::HashMap::from([
                    #(#custom_attrs),*
                ]);
                Some(starlark::values::docs::Doc {
                    id,
                    item,
                    custom_attrs,
                })
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
                lit: syn::Lit::Str(s),
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
        if attr.path.is_ident(STARLARK_DOCS_ATTRS) {
            return get_attrs(attr);
        }
    }

    Ok(HashMap::new())
}
