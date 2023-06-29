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
    /// Implement `UnpackValue` for `&T`.
    /// Note we are implementing `UnpackValue` for `&T` instead of `T`,
    /// therefore we use proc macro on impl trait instead of `#[derive]` on struct.
    unpack_value: bool,
    /// Implement `StarlarkTypeRepr` for `&T`.
    starlark_type_repr: bool,
}

impl syn::parse::Parse for StarlarkValueAttrs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<syn::Token![type]>()?;
        input.parse::<syn::Token![=]>()?;
        let typ = input.parse::<syn::Expr>()?;
        let mut attrs = StarlarkValueAttrs {
            typ,
            unpack_value: false,
            starlark_type_repr: false,
        };

        loop {
            if input.is_empty() {
                break;
            }
            input.parse::<syn::Token![,]>()?;
            if input.is_empty() {
                // Allow trailing comma.
                break;
            }
            let name = input.parse::<syn::Ident>()?;
            if name == "UnpackValue" {
                attrs.unpack_value = true;
            } else if name == "StarlarkTypeRepr" {
                attrs.starlark_type_repr = true;
            } else {
                return Err(syn::Error::new_spanned(
                    name,
                    "unknown attribute, allowed attribute is `UnpackValue`, `StarlarkTypeRepr`",
                ));
            }
        }

        Ok(attrs)
    }
}

struct ImplStarlarkValue<'a> {
    input: &'a syn::ItemImpl,
    lifetime_param: &'a syn::Lifetime,
}

fn is_impl_starlark_value(input: &syn::ItemImpl) -> syn::Result<ImplStarlarkValue> {
    let err = "expected `impl StarlarkValue for ...`";
    let Some((_, path, _)) = &input.trait_ else {
        return Err(syn::Error::new_spanned(
            input,
            err,
        ));
    };
    let Some(last) = path.segments.last() else {
        return Err(syn::Error::new_spanned(
            path,
            err,
        ));
    };
    if last.ident != "StarlarkValue" {
        return Err(syn::Error::new_spanned(&last.ident, err));
    }
    let mut lifetime_param = None;
    for lt in input.generics.lifetimes() {
        if lifetime_param.is_some() {
            return Err(syn::Error::new_spanned(
                lt,
                "multiple lifetime parameters are not supported",
            ));
        }
        lifetime_param = Some(lt);
    }
    let lifetime_param = match lifetime_param {
        Some(lt) => &lt.lifetime,
        None => {
            return Err(syn::Error::new_spanned(
                input,
                "expected a lifetime parameter",
            ));
        }
    };
    Ok(ImplStarlarkValue {
        input,
        lifetime_param,
    })
}

fn impl_unpack_value(
    input: &ImplStarlarkValue,
    unpack_value: bool,
    starlark_type_repr: bool,
) -> syn::Result<proc_macro2::TokenStream> {
    if !unpack_value && !starlark_type_repr {
        return Ok(proc_macro2::TokenStream::new());
    }

    if !unpack_value || !starlark_type_repr {
        return Err(syn::Error::new_spanned(
            input.input,
            "`UnpackValue` and `StarlarkTypeRepr` can only be specified together",
        ));
    }

    for param in &input.input.generics.params {
        match param {
            syn::GenericParam::Lifetime(_) => {}
            _ => {
                return Err(syn::Error::new_spanned(
                    param,
                    "only lifetime parameters are supported to implement `UnpackValue` or `StarlarkTypeRepr`",
                ));
            }
        }
    }

    let lt = input.lifetime_param;
    let params = &input.input.generics.params;
    let where_clause = &input.input.generics.where_clause;
    let self_ty = &input.input.self_ty;
    Ok(quote_spanned! {
        input.input.span() =>

        impl<#params> starlark::values::type_repr::StarlarkTypeRepr for &#lt #self_ty
        #where_clause
        {
            fn starlark_type_repr() -> starlark::typing::Ty {
                <#self_ty as starlark::values::type_repr::StarlarkTypeRepr>::starlark_type_repr()
            }
        }

        impl<#params> starlark::values::UnpackValue<#lt> for &#lt #self_ty
        #where_clause
        {
            fn unpack_value(value: starlark::values::Value<#lt>) -> Option<&#lt #self_ty> {
                starlark::values::ValueLike::downcast_ref(value)
            }
        }
    })
}

fn derive_starlark_value_impl(
    attr: StarlarkValueAttrs,
    mut input: syn::ItemImpl,
) -> syn::Result<proc_macro2::TokenStream> {
    let StarlarkValueAttrs {
        typ,
        unpack_value,
        starlark_type_repr,
    } = attr;

    let impl_starlark_value = is_impl_starlark_value(&input)?;

    let impl_unpack_value =
        impl_unpack_value(&impl_starlark_value, unpack_value, starlark_type_repr)?;

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

        #impl_unpack_value

        #input
    })
}
