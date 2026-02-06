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
use syn::ItemStruct;
use syn::ItemTrait;
use syn::Path;
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

/// Generate code for an impl block for a struct or impl declaration for a struct with `#[pagable_typetag]`.
///
/// This generates:
/// - `PagableTagged` impl for the concrete type
/// - `inventory::submit!` to register the type with the trait-specific registration struct
fn typetag_struct(
    item: TokenStream,
    self_ty: &syn::Type,
    trait_path: &syn::Path,
    type_tag: &str,
    span: Span,
) -> TokenStream {
    quote_spanned! { span =>
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
            let span = struct_item.span();
            let item_tokens = quote_spanned! { span => #struct_item };
            return typetag_struct(item_tokens, &self_ty, &trait_path, &type_tag, span).into();
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
            Some((_, path, _)) => path.clone(),
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
        let span = impl_item.span();
        let item_tokens = quote_spanned! { span => #impl_item };
        typetag_struct(item_tokens, &self_ty, &trait_path, &type_tag, span).into()
    } else {
        syn::Error::new(
            Span::call_site(),
            "pagable_typetag can only be applied to trait definitions, impl blocks, or struct definitions (with trait argument)",
        )
        .to_compile_error()
        .into()
    }
}
