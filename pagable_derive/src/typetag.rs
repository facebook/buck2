/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Proc macros for pagable typetag support.
//!
//! This module provides attribute macros similar to the `typetag` crate but
//! for pagable serialization.

use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use syn::Ident;
use syn::ItemImpl;
use syn::ItemStruct;
use syn::ItemTrait;
use syn::Path;

/// Generate code for a trait definition with `#[pagable_typetag]`.
///
/// This generates:
/// - A trait-specific registration struct
/// - A static registry for the trait
/// - `inventory::collect!` for the registration struct
/// - `PagableBoxDeserialize` impl for `dyn Trait`
pub fn typetag_trait(item: ItemTrait) -> syn::Result<TokenStream> {
    let trait_name = &item.ident;
    let trait_vis = &item.vis;
    let registration_struct_name = Ident::new(
        &format!("__PagableRegistration_{}", trait_name),
        Span::call_site(),
    );
    let accumulator_name = format_ident!("__PAGABLE_GENERIC_ACC_{}", trait_name);

    Ok(quote! {
        #item

        // Per-trait wrapper around TypetagRegistration to satisfy inventory orphan rules.
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        #trait_vis struct #registration_struct_name(
            pub pagable::typetag::TypetagRegistration<dyn #trait_name>
        );

        impl dyn #trait_name {
            #[doc(hidden)]
            #trait_vis const fn __pagable_wrap_registration(
                reg: pagable::typetag::TypetagRegistration<dyn #trait_name>,
            ) -> #registration_struct_name {
                #registration_struct_name(reg)
            }

            fn __pagable_registry() -> &'static pagable::typetag::TypetagRegistry<dyn #trait_name> {
                static REGISTRY: std::sync::OnceLock<
                    pagable::typetag::TypetagRegistry<dyn #trait_name>
                > = std::sync::OnceLock::new();
                REGISTRY.get_or_init(|| {
                    let generic_entries = #accumulator_name.drain();
                    pagable::typetag::TypetagRegistry::from_inventory_and_generic(
                        pagable::__internal::inventory::iter::<#registration_struct_name>
                            .into_iter()
                            .map(|r| &r.0),
                        generic_entries,
                    )
                })
            }
        }

        pagable::__internal::inventory::collect!(#registration_struct_name);

        // --- Generic typetag section infrastructure ---

        #[doc(hidden)]
        #[expect(non_upper_case_globals, reason = "generated from the trait name")]
        #trait_vis static #accumulator_name: pagable::typetag::GenericTypetagAccumulator<dyn #trait_name> =
            pagable::typetag::GenericTypetagAccumulator::new();

        impl<'de> pagable::PagableBoxDeserialize<'de> for dyn #trait_name {
            fn deserialize_box<D: pagable::PagableDeserializer<'de> + ?Sized>(
                deserializer: &mut D,
            ) -> pagable::Result<Box<Self>> {
                <dyn #trait_name>::__pagable_registry()
                    .deserialize_tagged(deserializer.as_dyn())
            }

            fn deserialize_arc_payload<D: pagable::PagableDeserializer<'de> + ?Sized>(
                deserializer: &mut D,
            ) -> pagable::Result<std::sync::Arc<Self>> {
                <dyn #trait_name>::__pagable_registry()
                    .deserialize_tagged_arc_payload(deserializer.as_dyn())
            }
        }

        // Write `tag + body` for a borrowed or boxed `dyn Trait`. The Arc-specific
        // override writes `tag + canonical concrete Arc` to preserve allocation identity.
        //
        // Allowed because `PagableTagged` doesn't have `PagableSerialize` as
        // a supertrait — otherwise Rust would auto-synthesize this impl and
        // hit E0371.
        impl pagable::PagableSerialize for dyn #trait_name {
            fn pagable_serialize(
                &self,
                serializer: &mut dyn pagable::PagableSerializer,
            ) -> pagable::Result<()> {
                pagable::typetag::PagableTagged::serialize_tagged(self, serializer)
            }

            fn pagable_serialize_arc_payload(
                self: std::sync::Arc<Self>,
                serializer: &mut dyn pagable::PagableSerializer,
            ) -> pagable::Result<()> {
                pagable::typetag::PagableTagged::serialize_tagged_arc_payload(self, serializer)
            }
        }
    })
}

/// Generate code for an impl block for a struct or impl declaration for a struct with `#[pagable_typetag]`.
///
/// This generates:
/// - `PagableTypeTag` impl for the concrete type
/// - `inventory::submit!` to register the type with the trait-specific registration struct
fn typetag_struct(
    item: TokenStream,
    self_ty: &syn::Type,
    trait_path: &syn::Path,
    type_tag: &str,
) -> TokenStream {
    quote! {
        #item

        // Implement PagableTypeTag for the concrete type
        impl pagable::typetag::PagableTypeTag for #self_ty {
            fn pagable_type_tag_static() -> &'static str {
                #type_tag
            }
        }

        // Direct (non-wrapped) typetag registrations deliberately do NOT emit
        // a `PagableRegisteredFor` impl: that marker is only meaningful when
        // paired with a wrapper type. Use `register_typetag!(Wrapper<T> as dyn Trait)`
        // to register `T` for a specific wrapper.

        // Submit to inventory for automatic registration,
        // using the per-trait wrapper struct generated by #[pagable_typetag] on the trait.
        pagable::__internal::inventory::submit! {
            <dyn #trait_path>::__pagable_wrap_registration(
                pagable::typetag::TypetagRegistration {
                    tag: <#self_ty as pagable::typetag::PagableTypeTag>::pagable_type_tag_static,
                    deserialize: |deserializer| {
                        let value: #self_ty =
                            pagable::PagableDeserialize::pagable_deserialize(deserializer)?;
                        Ok(Box::new(value) as Box<dyn #trait_path>)
                    },
                    deserialize_arc_payload: |deserializer| {
                        let value: std::sync::Arc<#self_ty> =
                            pagable::PagableDeserialize::pagable_deserialize(deserializer)?;
                        let value: std::sync::Arc<dyn #trait_path> = value;
                        Ok(value)
                    },
                }
            )
        }
    }
}

/// Main entry point for the `#[pagable_typetag]` attribute macro.
///
/// Supports three forms:
/// - `#[pagable_typetag]` on a trait definition
/// - `#[pagable_typetag]` on an impl block
/// - `#[pagable_typetag(TraitName)]` on a struct definition or concrete type
pub fn pagable_typetag_impl(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // Check if we have a trait path attribute (for struct declarations)
    let trait_path: Option<Path> = if attr.is_empty() {
        None
    } else {
        match syn::parse::<Path>(attr) {
            Ok(path) => Some(path),
            Err(err) => return err.to_compile_error().into(),
        }
    };

    // If we have a trait path try to parse as a struct
    if let Some(trait_path) = trait_path {
        if let Ok(struct_item) = syn::parse::<ItemStruct>(item.clone()) {
            let struct_name = &struct_item.ident;
            let type_tag = struct_name.to_string();
            let self_ty: syn::Type = syn::parse_quote!(#struct_name);
            let item_tokens = quote! { #struct_item };
            return typetag_struct(item_tokens, &self_ty, &trait_path, &type_tag).into();
        } else {
            return syn::Error::new(
                Span::call_site(),
                "pagable_typetag with a trait argument can only be applied to struct definitions",
            )
            .to_compile_error()
            .into();
        }
    }

    // Try to parse as a trait or impl block
    if let Ok(trait_item) = syn::parse::<ItemTrait>(item.clone()) {
        match typetag_trait(trait_item) {
            Ok(tokens) => tokens.into(),
            Err(err) => err.to_compile_error().into(),
        }
    } else if let Ok(impl_item) = syn::parse::<ItemImpl>(item) {
        let self_ty = (*impl_item.self_ty).clone();
        let trait_path = match impl_item.trait_.as_ref() {
            Some((path, _)) => path.clone(),
            None => {
                return syn::Error::new_spanned(
                    &impl_item,
                    "pagable_typetag requires a trait impl",
                )
                .to_compile_error()
                .into();
            }
        };
        let type_tag = if let syn::Type::Path(type_path) = &self_ty {
            match type_path.path.segments.last() {
                Some(seg) => seg.ident.to_string(),
                None => {
                    return syn::Error::new_spanned(
                        &self_ty,
                        "could not extract type name for pagable_typetag",
                    )
                    .to_compile_error()
                    .into();
                }
            }
        } else {
            return syn::Error::new_spanned(
                &self_ty,
                "pagable_typetag requires a simple type path",
            )
            .to_compile_error()
            .into();
        };
        let item_tokens = quote! { #impl_item };
        typetag_struct(item_tokens, &self_ty, &trait_path, &type_tag).into()
    } else {
        syn::Error::new(
            Span::call_site(),
            "pagable_typetag can only be applied to trait definitions, impl blocks, or struct definitions (with trait argument)",
        )
        .to_compile_error()
        .into()
    }
}
