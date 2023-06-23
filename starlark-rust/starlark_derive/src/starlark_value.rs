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
use syn::spanned::Spanned;

pub(crate) fn derive_starlark_value(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr = syn::parse_macro_input!(attr as StarlarkValueAttrs);
    let input = syn::parse_macro_input!(input as syn::ItemImpl);
    match derive_starlark_value_impl(attr, input) {
        Ok(gen) => gen.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

struct StarlarkValueAttrs {
    typ: syn::Expr,
}

impl syn::parse::Parse for StarlarkValueAttrs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<syn::Token![type]>()?;
        input.parse::<syn::Token![=]>()?;
        let typ = input.parse::<syn::Expr>()?;
        Ok(StarlarkValueAttrs { typ })
    }
}

fn is_impl_starlark_value(input: &syn::ItemImpl) -> bool {
    let Some((_, path, _)) = &input.trait_ else {
        return false;
    };
    let Some(last) = path.segments.last() else {
        return false;
    };
    last.ident == "StarlarkValue"
}

fn derive_starlark_value_impl(
    attr: StarlarkValueAttrs,
    mut input: syn::ItemImpl,
) -> syn::Result<proc_macro2::TokenStream> {
    let StarlarkValueAttrs { typ } = attr;

    if !is_impl_starlark_value(&input) {
        return Err(syn::Error::new_spanned(
            input,
            "#[starlark_value] can only be applied to `impl StarlarkValue for ...`",
        ));
    }

    let please_use_starlark_type_macro: syn::ImplItem =
        syn::parse2(quote_spanned! { input.span() =>
            fn please_use_starlark_type_macro() {}
        })?;

    let const_type: syn::ImplItem = syn::parse2(quote_spanned! { input.span() =>
        const TYPE: &'static str = #typ;
    })?;

    let get_type_value_static: syn::ImplItem = syn::parse2(quote_spanned! { input.span() =>
        #[inline]
        fn get_type_value_static() -> starlark::values::FrozenStringValue {
            starlark::const_frozen_string!(#typ)
        }
    })?;

    input.items.splice(
        0..0,
        [
            const_type,
            get_type_value_static,
            please_use_starlark_type_macro,
        ],
    );

    Ok(quote_spanned! {
        input.span() =>

        #input
    })
}
