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

use quote::quote_spanned;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::DeriveInput;

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
        ..
    } = input;

    Ok(quote_spanned! {span=>
        impl #generics #name #generics {
            #[doc(hidden)]
            pub fn __generated_documentation() -> Option<starlark::values::docs::Doc> {
                let name = <#name as starlark::values::StarlarkValue>::get_type_value_static().as_str().to_owned();
                let id = starlark::values::docs::Identifier {
                    name,
                    location: None,
                };
                let item = <#name as starlark::values::StarlarkValue>::get_methods()?.documentation();
                Some(starlark::values::docs::Doc {
                    id,
                    item,
                    custom_attrs: Default::default(),
                })
            }
        }
    })
}
