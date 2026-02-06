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
use quote::quote_spanned;
use syn::Ident;
use syn::ItemImpl;
use syn::ItemTrait;
use syn::spanned::Spanned;

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
    let registry_name = Ident::new(
        &format!(
            "__PAGABLE_REGISTRY_{}",
            trait_name.to_string().to_uppercase()
        ),
        Span::call_site(),
    );
    let registration_struct_name = Ident::new(
        &format!("__PagableRegistration_{}", trait_name),
        Span::call_site(),
    );

    Ok(quote_spanned! { item.span() =>
        #item

        // Define a registration struct specific to this trait - in the users crate
        // This avoids orphan rule issues with inventory::collect!
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        #trait_vis struct #registration_struct_name {
            pub tag: &'static str,
            pub deserialize: pagable::typetag::DeserializeFn<dyn #trait_name>,
        }

        // SAFETY: The deserialize function pointer is Send + Sync
        unsafe impl Send for #registration_struct_name {}
        unsafe impl Sync for #registration_struct_name {}

        // Collect registrations for this trait - works because the struct is defined here
        pagable::__internal::inventory::collect!(#registration_struct_name);

        // Provide a way to construct registration structs through the trait object type.
        // This allows impl blocks to use `<dyn TraitName>::__pagable_registration(...)`
        // without needing to import the registration struct directly.
        impl dyn #trait_name {
            #[doc(hidden)]
            #trait_vis const fn __pagable_registration(
                tag: &'static str,
                deserialize: pagable::typetag::DeserializeFn<dyn #trait_name>,
            ) -> #registration_struct_name {
                #registration_struct_name { tag, deserialize }
            }
        }

        // Define the registry that uses our custom registration struct
        #trait_vis static #registry_name: std::sync::OnceLock<pagable::typetag::Registry<dyn #trait_name>> = std::sync::OnceLock::new();

        fn __pagable_init_registry() -> &'static pagable::typetag::Registry<dyn #trait_name> {
            #registry_name.get_or_init(|| {
                let mut map = std::collections::HashMap::new();
                for registered in pagable::__internal::inventory::iter::<#registration_struct_name> {
                    map.insert(registered.tag, registered.deserialize);
                }
                pagable::typetag::Registry(map)
            })
        }

        impl<'de> pagable::PagableBoxDeserialize<'de> for dyn #trait_name {
            fn deserialize_box<D: pagable::PagableDeserializer<'de> + ?Sized>(
                deserializer: &mut D,
            ) -> pagable::Result<Box<Self>> {
                let mut deserializer = deserializer;
                __pagable_init_registry().deserialize_tagged(&mut deserializer)
            }
        }
    })
}

/// Generate code for an impl block with `#[pagable_typetag]`.
///
/// This generates:
/// - `PagableTagged` impl for the concrete type
/// - `inventory::submit!` to register the type with the trait-specific registration struct
pub fn typetag_impl(item: ItemImpl) -> syn::Result<TokenStream> {
    // Extract the concrete type being implemented
    let self_ty = &item.self_ty;

    // Extract the trait being implemented
    let trait_path = item
        .trait_
        .as_ref()
        .map(|(_, path, _)| path)
        .ok_or_else(|| syn::Error::new_spanned(&item, "pagable_typetag requires a trait impl"))?;

    // Get the type name for the tag from the type path
    let type_tag = if let syn::Type::Path(type_path) = self_ty.as_ref() {
        type_path
            .path
            .segments
            .last()
            .map(|seg| seg.ident.to_string())
            .ok_or_else(|| {
                syn::Error::new_spanned(self_ty, "could not extract type name for pagable_typetag")
            })?
    } else {
        return Err(syn::Error::new_spanned(
            self_ty,
            "pagable_typetag requires a simple type path",
        ));
    };

    Ok(quote_spanned! { item.span() =>
        #item

        // Implement PagableTagged for the concrete type
        impl pagable::typetag::PagableTagged for #self_ty {
            fn pagable_type_tag(&self) -> &'static str {
                #type_tag
            }
        }

        // Submit to inventory for automatic registration.
        // Uses the trait object's inherent method to construct the registration struct,
        // so users only need the trait in scope, not the registration struct itself.
        pagable::__internal::inventory::submit! {
            <dyn #trait_path>::__pagable_registration(
                #type_tag,
                |deserializer| {
                    let value: #self_ty =
                        pagable::PagableDeserialize::pagable_deserialize(deserializer)?;
                    Ok(Box::new(value) as Box<dyn #trait_path>)
                },
            )
        }
    })
}

/// Main entry point for the `#[pagable_typetag]` attribute macro.
pub fn pagable_typetag_impl(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // Reject any attributes - none are supported
    if !attr.is_empty() {
        return syn::Error::new(
            Span::call_site(),
            "pagable_typetag does not accept any attributes",
        )
        .to_compile_error()
        .into();
    }

    // Try to parse as a trait first
    if let Ok(trait_item) = syn::parse::<ItemTrait>(item.clone()) {
        match typetag_trait(trait_item) {
            Ok(tokens) => tokens.into(),
            Err(err) => err.to_compile_error().into(),
        }
    } else if let Ok(impl_item) = syn::parse::<ItemImpl>(item) {
        match typetag_impl(impl_item) {
            Ok(tokens) => tokens.into(),
            Err(err) => err.to_compile_error().into(),
        }
    } else {
        syn::Error::new(
            Span::call_site(),
            "pagable_typetag can only be applied to trait definitions or impl blocks",
        )
        .to_compile_error()
        .into()
    }
}
