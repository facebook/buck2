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

use proc_macro2::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::DeriveInput;

fn punctuated_try_map<A, B, P: Clone>(
    punctuated: &Punctuated<A, P>,
    f: impl Fn(&A) -> syn::Result<B>,
) -> syn::Result<Punctuated<B, P>> {
    let mut new_punctuated = Punctuated::new();
    for pair in punctuated.pairs() {
        new_punctuated.push_value(f(pair.value())?);
        if let Some(p) = pair.punct() {
            new_punctuated.push_punct((*p).clone());
        }
    }
    Ok(new_punctuated)
}

fn generic_argument_replace_lifetimes_with_static(
    generic_argument: &syn::GenericArgument,
) -> syn::Result<syn::GenericArgument> {
    match generic_argument {
        syn::GenericArgument::Lifetime(lifetime) => Ok(syn::GenericArgument::Lifetime(
            syn::Lifetime::new("'static", lifetime.span()),
        )),
        a @ syn::GenericArgument::Const(..) => Ok(a.clone()),
        c => Err(syn::Error::new_spanned(c, "unsupported generic argument")),
    }
}

fn path_arguments_replace_lifetimes_with_static(
    path_arguments: &syn::PathArguments,
) -> syn::Result<syn::PathArguments> {
    Ok(match path_arguments {
        syn::PathArguments::None => syn::PathArguments::None,
        syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
            colon2_token,
            lt_token,
            args,
            gt_token,
        }) => syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
            colon2_token: *colon2_token,
            lt_token: *lt_token,
            args: punctuated_try_map(args, generic_argument_replace_lifetimes_with_static)?,
            gt_token: *gt_token,
        }),
        syn::PathArguments::Parenthesized(_) => {
            return Err(syn::Error::new_spanned(
                path_arguments,
                "unsupported path arguments",
            ));
        }
    })
}

fn path_segment_replace_lifetimes_with_static(
    path_segment: &syn::PathSegment,
) -> syn::Result<syn::PathSegment> {
    Ok(syn::PathSegment {
        ident: path_segment.ident.clone(),
        arguments: path_arguments_replace_lifetimes_with_static(&path_segment.arguments)?,
    })
}

fn path_replace_lifetimes_with_static(path: &syn::Path) -> syn::Result<syn::Path> {
    Ok(syn::Path {
        leading_colon: path.leading_colon,
        segments: punctuated_try_map(&path.segments, path_segment_replace_lifetimes_with_static)?,
    })
}

/// Replace `X<'a>` with `X<'static>`.
fn type_param_bound_replace_lifetimes_with_static(
    bound: &syn::TypeParamBound,
) -> syn::Result<syn::TypeParamBound> {
    match bound {
        syn::TypeParamBound::Lifetime(lifetime) => Ok(syn::TypeParamBound::Lifetime(
            syn::Lifetime::new("'static", lifetime.span()),
        )),
        syn::TypeParamBound::Trait(trait_bound) => {
            if trait_bound.lifetimes.is_some() {
                return Err(syn::Error::new_spanned(
                    trait_bound,
                    "trait bounds with generic lifetimes are not supported",
                ));
            }
            Ok(syn::TypeParamBound::Trait(syn::TraitBound {
                paren_token: trait_bound.paren_token,
                modifier: trait_bound.modifier,
                lifetimes: trait_bound.lifetimes.clone(),
                path: path_replace_lifetimes_with_static(&trait_bound.path)?,
            }))
        }
        _ => Ok(bound.clone()),
    }
}

pub(crate) fn derive_provides_static_type(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match derive_provides_static_type_impl(input) {
        Ok(gen) => gen,
        Err(e) => e.to_compile_error().into(),
    }
}

/// Single lifetime parameter for `ProvidesStaticType`
fn pst_lifetime<'a>(
    params: impl Iterator<Item = &'a syn::GenericParam>,
) -> syn::Result<TokenStream> {
    let mut lifetime = None;
    for param in params {
        if let syn::GenericParam::Lifetime(param) = param {
            if lifetime.is_some() {
                return Err(syn::Error::new_spanned(
                    param,
                    "only one lifetime parameter is supported",
                ));
            }
            lifetime = Some(param);
        }
    }
    Ok(match lifetime {
        Some(lifetime) => quote! { #lifetime },
        None => quote! { 'pst },
    })
}

fn derive_provides_static_type_impl(
    input: proc_macro::TokenStream,
) -> syn::Result<proc_macro::TokenStream> {
    let input: DeriveInput = syn::parse(input)?;

    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let lifetime = pst_lifetime(input.generics.params.iter())?;

    let mut lifetimes = Vec::new();
    let mut static_lifetimes = Vec::new();
    let mut type_param_names = Vec::new();
    let mut type_param_bounds = Vec::new();
    let mut type_param_static_type_bounds = Vec::new();
    let mut static_type_params = Vec::new();
    let mut const_params = Vec::new();
    let mut const_param_names = Vec::new();
    for param in &input.generics.params {
        match param {
            syn::GenericParam::Lifetime(param) => {
                lifetimes.push(param.lifetime.clone());
                static_lifetimes.push(quote! {'static});
            }
            syn::GenericParam::Type(param) => {
                let has_static_lifetime_bound = param.bounds.iter().any(|bound| {
                    if let syn::TypeParamBound::Lifetime(lifetime) = bound {
                        lifetime.ident == "static"
                    } else {
                        false
                    }
                });

                let param_name = &param.ident;
                let param_bounds = param.bounds.iter();
                if has_static_lifetime_bound {
                    type_param_bounds.push(quote! {
                        #param_name : #(#param_bounds+)* Sized
                    });
                    let param_bounds = param
                        .bounds
                        .iter()
                        .map(type_param_bound_replace_lifetimes_with_static)
                        .collect::<syn::Result<Vec<_>>>()?;
                    type_param_static_type_bounds.push(quote! {
                        #param_name : #(#param_bounds+)* Sized
                    });
                    static_type_params.push(quote! { #param_name});
                } else {
                    type_param_bounds.push(quote! {
                        #param_name : #(#param_bounds+)* starlark::any::ProvidesStaticType<#lifetime> + Sized
                    });
                    let param_bounds = param
                        .bounds
                        .iter()
                        .map(type_param_bound_replace_lifetimes_with_static)
                        .collect::<syn::Result<Vec<_>>>()?;
                    type_param_static_type_bounds.push(quote! {
                        #param_name :: StaticType : #(#param_bounds+)* Sized
                    });
                    static_type_params.push(quote! { #param_name :: StaticType });
                }
                type_param_names.push(param.ident.clone());
            }
            syn::GenericParam::Const(params) => {
                const_params.push(params.clone());
                const_param_names.push(params.ident.clone());
            }
        }
    }

    let gen = if input.generics.lt_token.is_none() {
        quote! {
            unsafe impl<#lifetime> #impl_generics starlark::any::ProvidesStaticType<#lifetime> for #name #ty_generics #where_clause {
                type StaticType = #name #ty_generics;
            }
        }
    } else {
        quote! {
            unsafe impl <
                #lifetime,
                #(#type_param_bounds,)*
                #(#const_params,)*
                    > starlark::any::ProvidesStaticType<#lifetime>
            for #name <
                #(#lifetimes,)*
                #(#type_param_names,)*
                #(#const_param_names,)*
                    > #where_clause
            where
                #(#type_param_static_type_bounds,)*
            {
                type StaticType = #name <
                    #(#static_lifetimes,)*
                    #(#static_type_params,)*
                    #(#const_param_names,)*
                        >;
            }
        }
    };

    Ok(gen.into())
}
