/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// This code is adapted from https://github.com/dtolnay/thiserror licensed under Apache-2.0 or MIT.

use std::collections::BTreeSet as Set;

use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use quote::quote_spanned;
use quote::ToTokens;
use syn::spanned::Spanned;
use syn::Data;
use syn::DeriveInput;
use syn::Member;
use syn::Result;
use syn::Token;
use syn::Visibility;

use crate::ast::Enum;
use crate::ast::Field;
use crate::ast::Input;
use crate::ast::Struct;
use crate::attr::Trait;
use crate::generics::InferredBounds;

pub fn derive(node: &DeriveInput) -> Result<TokenStream> {
    let input = Input::from_syn(node)?;
    input.validate()?;
    Ok(match input {
        Input::Struct(input) => impl_struct(input),
        Input::Enum(input) => impl_enum(input),
    })
}

fn impl_struct(input: Struct) -> TokenStream {
    let ty = &input.ident;
    let (impl_generics, ty_generics, _) = input.generics.split_for_impl();
    let mut error_inferred_bounds = InferredBounds::new();

    let source_body = if input.attrs.transparent.is_some() {
        let only_field = &input.fields[0];
        if only_field.contains_generic {
            error_inferred_bounds.insert(only_field.ty, quote!(std::error::Error));
        }
        let member = &only_field.member;
        Some(quote! {
            std::error::Error::source(self.#member.as_dyn_error())
        })
    } else if let Some(source_field) = input.source_field() {
        let source = &source_field.member;
        if source_field.contains_generic {
            error_inferred_bounds.insert(source_field.ty, quote!(std::error::Error + 'static));
        }
        let dyn_error = quote_spanned!(source.span()=> self.#source .as_dyn_error());
        Some(quote! {
            std::option::Option::Some(#dyn_error)
        })
    } else {
        None
    };
    let source_method = source_body.map(|body| {
        quote! {
            fn source(&self) -> std::option::Option<&(dyn std::error::Error + 'static)> {
                use thiserror::__private::AsDynError;
                #body
            }
        }
    });

    let mut display_implied_bounds = Set::new();
    let display_body = if input.attrs.transparent.is_some() {
        let only_field = &input.fields[0].member;
        display_implied_bounds.insert((0, Trait::Display));
        Some(quote! {
            std::fmt::Display::fmt(&self.#only_field, __formatter)
        })
    } else if let Some(display) = &input.attrs.display {
        display_implied_bounds = display.implied_bounds.clone();
        let pat = fields_pat(&input.fields);
        Some(quote! {
            #[allow(unused_variables, deprecated)]
            let Self #pat = self;
            #display
        })
    } else {
        None
    };
    let display_impl = display_body.map(|body| {
        let mut display_inferred_bounds = InferredBounds::new();
        for (field, bound) in display_implied_bounds {
            let field = &input.fields[field];
            if field.contains_generic {
                display_inferred_bounds.insert(field.ty, bound);
            }
        }
        let display_where_clause = display_inferred_bounds.augment_where_clause(input.generics);
        quote! {
            #[allow(unused_qualifications)]
            impl #impl_generics std::fmt::Display for #ty #ty_generics #display_where_clause {
                #[allow(clippy::used_underscore_binding)]
                fn fmt(&self, __formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    #body
                }
            }
        }
    });

    let error_trait = spanned_error_trait(input.original);
    if input.generics.type_params().next().is_some() {
        let self_token = <Token![Self]>::default();
        error_inferred_bounds.insert(self_token, Trait::Debug);
        error_inferred_bounds.insert(self_token, Trait::Display);
    }
    let error_where_clause = error_inferred_bounds.augment_where_clause(input.generics);

    quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics #error_trait for #ty #ty_generics #error_where_clause {
            #source_method
        }
        #display_impl
    }
}

fn impl_enum(input: Enum) -> TokenStream {
    let ty = &input.ident;
    let (impl_generics, ty_generics, _) = input.generics.split_for_impl();
    let mut error_inferred_bounds = InferredBounds::new();

    let source_method = if input.has_source() {
        let arms = input.variants.iter().map(|variant| {
            let ident = &variant.ident;
            if variant.attrs.transparent.is_some() {
                let only_field = &variant.fields[0];
                if only_field.contains_generic {
                    error_inferred_bounds.insert(only_field.ty, quote!(std::error::Error));
                }
                let member = &only_field.member;
                let source = quote!(std::error::Error::source(transparent.as_dyn_error()));
                quote! {
                    #ty::#ident {#member: transparent} => #source,
                }
            } else if let Some(source_field) = variant.source_field() {
                let source = &source_field.member;
                if source_field.contains_generic {
                    error_inferred_bounds
                        .insert(source_field.ty, quote!(std::error::Error + 'static));
                }
                let varsource = quote!(source);
                let dyn_error = quote_spanned!(source.span()=> #varsource.as_dyn_error());
                quote! {
                    #ty::#ident {#source: #varsource, ..} => std::option::Option::Some(#dyn_error),
                }
            } else {
                quote! {
                    #ty::#ident {..} => std::option::Option::None,
                }
            }
        });
        Some(quote! {
            fn source(&self) -> std::option::Option<&(dyn std::error::Error + 'static)> {
                use thiserror::__private::AsDynError;
                #[allow(deprecated)]
                match self {
                    #(#arms)*
                }
            }
        })
    } else {
        None
    };

    let display_impl = if input.has_display() {
        let mut display_inferred_bounds = InferredBounds::new();
        let void_deref = if input.variants.is_empty() {
            Some(quote!(*))
        } else {
            None
        };
        let arms = input.variants.iter().map(|variant| {
            let mut display_implied_bounds = Set::new();
            let display = match &variant.attrs.display {
                Some(display) => {
                    display_implied_bounds = display.implied_bounds.clone();
                    display.to_token_stream()
                }
                None => {
                    let only_field = match &variant.fields[0].member {
                        Member::Named(ident) => ident.clone(),
                        Member::Unnamed(index) => format_ident!("_{}", index),
                    };
                    display_implied_bounds.insert((0, Trait::Display));
                    quote!(std::fmt::Display::fmt(#only_field, __formatter))
                }
            };
            for (field, bound) in display_implied_bounds {
                let field = &variant.fields[field];
                if field.contains_generic {
                    display_inferred_bounds.insert(field.ty, bound);
                }
            }
            let ident = &variant.ident;
            let pat = fields_pat(&variant.fields);
            quote! {
                #ty::#ident #pat => #display
            }
        });
        let arms = arms.collect::<Vec<_>>();
        let display_where_clause = display_inferred_bounds.augment_where_clause(input.generics);
        Some(quote! {
            #[allow(unused_qualifications)]
            impl #impl_generics std::fmt::Display for #ty #ty_generics #display_where_clause {
                fn fmt(&self, __formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    #[allow(unused_variables, deprecated, clippy::used_underscore_binding)]
                    match #void_deref self {
                        #(#arms,)*
                    }
                }
            }
        })
    } else {
        None
    };

    let error_trait = spanned_error_trait(input.original);
    if input.generics.type_params().next().is_some() {
        let self_token = <Token![Self]>::default();
        error_inferred_bounds.insert(self_token, Trait::Debug);
        error_inferred_bounds.insert(self_token, Trait::Display);
    }
    let error_where_clause = error_inferred_bounds.augment_where_clause(input.generics);

    quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics #error_trait for #ty #ty_generics #error_where_clause {
            #source_method
        }
        #display_impl
    }
}

fn fields_pat(fields: &[Field]) -> TokenStream {
    let mut members = fields.iter().map(|field| &field.member).peekable();
    match members.peek() {
        Some(Member::Named(_)) => quote!({ #(#members),* }),
        Some(Member::Unnamed(_)) => {
            let vars = members.map(|member| match member {
                Member::Unnamed(member) => format_ident!("_{}", member),
                Member::Named(_) => unreachable!(),
            });
            quote!((#(#vars),*))
        }
        None => quote!({}),
    }
}

fn spanned_error_trait(input: &DeriveInput) -> TokenStream {
    let vis_span = match &input.vis {
        Visibility::Public(vis) => Some(vis.span),
        Visibility::Restricted(vis) => Some(vis.pub_token.span),
        Visibility::Inherited => None,
    };
    let data_span = match &input.data {
        Data::Struct(data) => data.struct_token.span,
        Data::Enum(data) => data.enum_token.span,
        Data::Union(data) => data.union_token.span,
    };
    let first_span = vis_span.unwrap_or(data_span);
    let last_span = input.ident.span();
    let path = quote_spanned!(first_span=> std::error::);
    let error = quote_spanned!(last_span=> Error);
    quote!(#path #error)
}
