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

//! `#[starlark_pagable_typetag]` — starlark-flavored `#[pagable_typetag]`.
//!
//! On a trait: emits a sealed marker (`dyn Trait: StarlarkTypetagTraitMarker`).
//! On `impl Trait for Foo`: emits the recovery bridge and asserts the marker.

use proc_macro2::Span;
use quote::quote_spanned;
use syn::ItemImpl;
use syn::ItemTrait;
use syn::spanned::Spanned;

/// Entry point — dispatches on whether `item` is a trait def or an impl block.
pub fn starlark_pagable_typetag_impl(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    if let Ok(trait_item) = syn::parse::<ItemTrait>(item.clone()) {
        return expand_trait(trait_item).into();
    }
    if let Ok(impl_item) = syn::parse::<ItemImpl>(item) {
        return expand_impl(impl_item);
    }
    syn::Error::new(
        Span::call_site(),
        "starlark_pagable_typetag can only be applied to trait definitions or `impl Trait for Type` blocks",
    )
    .to_compile_error()
    .into()
}

fn expand_trait(trait_item: ItemTrait) -> proc_macro2::TokenStream {
    let trait_name = &trait_item.ident;
    let span = trait_item.span();
    quote_spanned! { span =>
        #[pagable::pagable_typetag]
        #trait_item

        // Sealed marker — only this macro can apply it. The impl-form
        // requires it, so impls of an unmarked trait fail to compile.
        const _: () = {
            impl starlark::pagable::__private::StarlarkTypetagTraitSealed
                for dyn #trait_name {}
            impl starlark::pagable::StarlarkTypetagTraitMarker
                for dyn #trait_name {}
        };
    }
}

fn expand_impl(impl_item: ItemImpl) -> proc_macro::TokenStream {
    let trait_path = match impl_item.trait_.as_ref() {
        Some((_, path, _)) => path.clone(),
        None => {
            return syn::Error::new_spanned(
                &impl_item,
                "starlark_pagable_typetag on an impl requires a trait impl (`impl Trait for Type`)",
            )
            .to_compile_error()
            .into();
        }
    };
    let self_ty = (*impl_item.self_ty).clone();
    let span = impl_item.span();

    quote_spanned! { span =>
        #[pagable::pagable_typetag]
        #impl_item

        // Marker check: requires `#[starlark_pagable_typetag]` on the trait.
        const _: () = {
            fn _assert_trait_marked_starlark()
            where
                dyn #trait_path: starlark::pagable::StarlarkTypetagTraitMarker,
            {
            }
        };

        // Recovery bridge: enter starlark ctx, delegate to StarlarkSerialize.
        impl pagable::PagableSerialize for #self_ty {
            fn pagable_serialize(
                &self,
                serializer: &mut dyn pagable::PagableSerializer,
            ) -> pagable::Result<()> {
                let mut ctx =
                    starlark::pagable::StarlarkSerializerImpl::recover_from_pagable(
                        serializer,
                    )
                    .map_err(|e: starlark::Error| e.into_anyhow())?;
                <Self as starlark::pagable::StarlarkSerialize>::starlark_serialize(
                    self,
                    &mut ctx,
                )
                .map_err(|e: starlark::Error| e.into_anyhow())
            }
        }

        impl<'de> pagable::PagableDeserialize<'de> for #self_ty {
            fn pagable_deserialize<D: pagable::PagableDeserializer<'de> + ?Sized>(
                deserializer: &mut D,
            ) -> pagable::Result<Self> {
                let mut ctx =
                    starlark::pagable::StarlarkDeserializerImpl::recover_from_pagable(
                        deserializer.as_dyn(),
                    )
                    .map_err(|e: starlark::Error| e.into_anyhow())?;
                <Self as starlark::pagable::StarlarkDeserialize>::starlark_deserialize(
                    &mut ctx,
                )
                .map_err(|e: starlark::Error| e.into_anyhow())
            }
        }
    }
    .into()
}
