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

//! The brand lifetime the pagable derives implement `StarlarkDeserialize` at.

use syn::DeriveInput;
use syn::Generics;
use syn::Lifetime;
use syn::LifetimeParam;
use syn::Token;
use syn::parse::ParseStream;
use syn::parse_quote;

/// What to do with the `starlark_pagable` arguments other than `brand`.
#[derive(Copy, Clone)]
pub(crate) enum OtherArgs {
    /// Skip them: the `StarlarkPagable` derive parses them itself.
    Skip,
    /// Reject them: nothing on this derive reads them, so any is a mistake.
    Reject,
}

/// Parse `#[starlark_pagable(brand = 'x)]` off the type's attributes.
pub(crate) fn extract_brand_attr(
    input: &DeriveInput,
    other_args: OtherArgs,
) -> syn::Result<Option<Lifetime>> {
    syn::custom_keyword!(brand);

    let mut brand_lifetime = None;
    for attr in &input.attrs {
        if !attr.path().is_ident("starlark_pagable") {
            continue;
        }
        attr.parse_args_with(|input: ParseStream| {
            while !input.is_empty() {
                if input.peek(brand) {
                    input.parse::<brand>()?;
                    input.parse::<Token![=]>()?;
                    let lifetime: Lifetime = input.parse()?;
                    if brand_lifetime.is_some() {
                        return Err(input.error("`brand` was set twice"));
                    }
                    brand_lifetime = Some(lifetime);
                } else if let OtherArgs::Reject = other_args {
                    return Err(input.error(
                        "unknown `starlark_pagable` argument: this derive reads only `brand = 'x`",
                    ));
                } else {
                    // Another argument, possibly with a `= value`; skip it.
                    input.step(|cursor| {
                        let mut rest = *cursor;
                        while let Some((tt, next)) = rest.token_tree() {
                            if let proc_macro2::TokenTree::Punct(p) = &tt
                                && p.as_char() == ','
                            {
                                return Ok(((), rest));
                            }
                            rest = next;
                        }
                        Ok(((), rest))
                    })?;
                }
                if input.is_empty() {
                    break;
                }
                input.parse::<Token![,]>()?;
            }
            Ok(())
        })?;
    }
    Ok(brand_lifetime)
}

/// The brand a type is deserialized at, and the generics of the `StarlarkDeserialize` impl.
///
/// A type's one lifetime parameter is its brand: `impl<'v> StarlarkDeserialize<'v> for Foo<'v>`.
/// A type without lifetime parameters holds no branded value and is deserializable at every
/// brand: `impl<'fv> StarlarkDeserialize<'fv> for Bar`, with `'fv` added to the impl generics.
/// A type with several lifetime parameters must name its brand with
/// `#[starlark_pagable(brand = 'x)]`.
pub(crate) fn deserialize_brand(
    input: &DeriveInput,
    other_args: OtherArgs,
) -> syn::Result<(Lifetime, Generics)> {
    let explicit = extract_brand_attr(input, other_args)?;
    let lifetimes: Vec<&LifetimeParam> = input.generics.lifetimes().collect();
    if let Some(explicit) = explicit {
        if !lifetimes.iter().any(|param| param.lifetime == explicit) {
            return Err(syn::Error::new_spanned(
                &explicit,
                "`brand` must name one of the type's lifetime parameters",
            ));
        }
        return Ok((explicit, input.generics.clone()));
    }
    match lifetimes.as_slice() {
        [] => {
            let lifetime: Lifetime = parse_quote!('fv);
            let mut generics = input.generics.clone();
            generics.params.insert(0, parse_quote!(#lifetime));
            Ok((lifetime, generics))
        }
        [param] => Ok((param.lifetime.clone(), input.generics.clone())),
        _ => Err(syn::Error::new_spanned(
            &input.generics,
            "a type with more than one lifetime parameter must say which one is its brand \
             with `#[starlark_pagable(brand = 'x)]`",
        )),
    }
}
