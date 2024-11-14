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

use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use quote::quote_spanned;
use quote::ToTokens;
use syn::spanned::Spanned;
use syn::Data;
use syn::DeriveInput;
use syn::Ident;
use syn::Member;
use syn::Result;
use syn::Token;
use syn::Visibility;

use crate::ast::Enum;
use crate::ast::Field;
use crate::ast::Input;
use crate::ast::Struct;
use crate::attr::Attrs;
use crate::attr::OptionStyle;
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
                use buck2_error::__for_macro::AsDynError;
                #body
            }
        }
    });

    let provide_body = gen_provide_contents(&input.attrs, &input.fields, ty, None);
    let pat = fields_pat(&input.fields);
    let provide_method = quote! {
        fn provide<'__macro>(&'__macro self, __request: &mut std::error::Request<'__macro>) {
            #[allow(unused_variables, deprecated)]
            let Self #pat = self;
            #provide_body
        }
    };

    let mut display_implied_bounds = Set::new();
    let display_body = if input.attrs.transparent.is_some() {
        let only_field = &input.fields[0].member;
        display_implied_bounds.insert((0, Trait::Display));
        Some(quote! {
            std::fmt::Display::fmt(&self.#only_field, __formatter)
        })
    } else if let Some(display) = &input.attrs.display {
        display_implied_bounds = display.implied_bounds.clone();
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
        error_inferred_bounds.insert(self_token, quote!(std::marker::Send));
        error_inferred_bounds.insert(self_token, quote!(std::marker::Sync));
        error_inferred_bounds.insert(self_token, quote!('static));
    }
    let error_where_clause = error_inferred_bounds.augment_where_clause(input.generics);

    quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics #error_trait for #ty #ty_generics #error_where_clause {
            #source_method
            #provide_method
        }
        #display_impl
    }
}

fn impl_enum(mut input: Enum) -> TokenStream {
    let ty = &input.ident;
    let (impl_generics, ty_generics, _) = input.generics.split_for_impl();
    let mut error_inferred_bounds = InferredBounds::new();

    // We let people specify these on the type or variant, so make sure that they always show up on
    // the variant and we don't have to re-check the type
    for variant in &mut input.variants {
        variant.attrs.tags.extend(input.attrs.tags.iter().cloned());
    }

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
                use buck2_error::__for_macro::AsDynError;
                #[allow(deprecated)]
                match self {
                    #(#arms)*
                }
            }
        })
    } else {
        None
    };

    let provide_arms = input.variants.iter().map(|variant| {
        let content =
            gen_provide_contents(&variant.attrs, &variant.fields, ty, Some(&variant.ident));
        let ident = &variant.ident;
        let pat = fields_pat(&variant.fields);
        quote! {
            #[allow(unused_variables, deprecated)]
            #ty::#ident #pat => {
                #content
            },
        }
    });
    let provide_method = quote! {
        fn provide<'__macro>(&'__macro self, __request: &mut std::error::Request<'__macro>) {
            match self {
                #(#provide_arms)*
            }
        }
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

            impl #impl_generics From<#ty #ty_generics> for buck2_error::__for_macro::ContextValue #display_where_clause  {
                fn from(value: #ty #ty_generics) -> buck2_error::__for_macro::ContextValue {
                    format!("{}", value).into()
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
        error_inferred_bounds.insert(self_token, quote!(std::marker::Send));
        error_inferred_bounds.insert(self_token, quote!(std::marker::Sync));
        error_inferred_bounds.insert(self_token, quote!('static));
    }
    let error_where_clause = error_inferred_bounds.augment_where_clause(input.generics);

    quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics #error_trait for #ty #ty_generics #error_where_clause {
            #source_method
            #provide_method
        }
        #display_impl
    }
}

/// Generates the provided data for either a variant or the whole struct
fn gen_provide_contents(
    attrs: &Attrs,
    fields: &[Field],
    type_name: &Ident,
    variant_name: Option<&Ident>,
) -> syn::Stmt {
    let type_and_variant = match variant_name {
        Some(variant_name) => format!("{}::{}", type_name, variant_name),
        None => type_name.to_string(),
    };
    let source_location_extra = syn::LitStr::new(&type_and_variant, Span::call_site());
    let tags: Vec<syn::Expr> = attrs
        .tags
        .iter()
        .map(|tag| match tag {
            OptionStyle::Explicit(tag) => syn::parse_quote! {
                core::option::Option::Some(buck2_error::ErrorTag::#tag)
            },
            OptionStyle::ByExpr(e) => e.clone(),
        })
        .collect();
    let num_tags = syn::LitInt::new(&format!("{}", tags.len()), Span::call_site());

    let metadata: syn::Stmt = syn::parse_quote! {
        buck2_error::provide_metadata(
            __request,
            <[Option<buck2_error::ErrorTag>; #num_tags] as IntoIterator>::into_iter([#(#tags,)*]).flatten(),
            core::file!(),
            core::option::Option::Some(#source_location_extra),
            core::option::Option::None,
        );
    };

    let forward_transparent = if attrs.transparent.is_some() {
        let only_field = match &fields[0].member {
            Member::Named(ident) => ident.clone(),
            Member::Unnamed(index) => format_ident!("_{}", index),
        };
        quote! {
            use buck2_error::__for_macro::AsDynError;
            std::error::Error::provide(#only_field.as_dyn_error(), __request);
        }
    } else {
        quote! {}
    };
    // When the same type is provided to the `request` more than once, the first value is used and
    // later values are ignored. As such, make sure we put the `forward_transparent` first, so that
    // if the underlying error has metadata, that's the one that gets used
    syn::parse_quote! {
        {
            #forward_transparent
            #metadata
        }
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
