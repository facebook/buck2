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

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse::ParseStream, parse_macro_input, spanned::Spanned, Attribute, Data, DataEnum, DataStruct,
    DeriveInput, Error, Fields, GenericParam, Index, LitStr, Token, Variant, WherePredicate,
};

struct Input<'a> {
    input: &'a DeriveInput,
}

impl<'a> Input<'a> {
    fn angle_brankets(tokens: &[TokenStream]) -> TokenStream {
        if tokens.is_empty() {
            quote! {}
        } else {
            quote! { < #(#tokens,)* > }
        }
    }

    fn format_impl_generics(
        &self,
        bounds: bool,
    ) -> syn::Result<(TokenStream, TokenStream, TokenStream)> {
        let mut impl_params = Vec::new();
        let mut input_params = Vec::new();
        let mut output_params = Vec::new();
        if bounds {
            impl_params.push(quote!('freeze));
        }
        for param in &self.input.generics.params {
            match param {
                GenericParam::Type(t) => {
                    let name = &t.ident;
                    let bounds = t.bounds.iter();
                    impl_params.push(quote! {
                        #name: #(#bounds +)* starlark::values::Freeze
                    });
                    input_params.push(quote! {
                        #name
                    });
                    output_params.push(quote! {
                        <#name as starlark::values::Freeze>::Frozen
                    });
                }
                GenericParam::Lifetime(lt) => {
                    impl_params.push(quote! { #lt });
                    input_params.push(quote! { #lt });
                    output_params.push(quote! { 'static });
                }
                GenericParam::Const(_) => {
                    return Err(Error::new_spanned(param, "const generics not supported"));
                }
            }
        }
        Ok((
            Self::angle_brankets(&impl_params),
            Self::angle_brankets(&input_params),
            Self::angle_brankets(&output_params),
        ))
    }
}

fn derive_freeze_impl(input: DeriveInput) -> syn::Result<TokenStream> {
    let input = Input { input: &input };

    let name = &input.input.ident;

    let opts = extract_options(&input.input.attrs)?;
    let (impl_params, input_params, output_params) =
        input.format_impl_generics(opts.bounds.is_some())?;

    let validate_body = match opts.validator {
        Some(validator) => quote! {
            #validator(&frozen)?;
        },
        None => quote! {},
    };

    let bounds_body = match opts.bounds {
        Some(bounds) => quote! { where #bounds },
        None => quote!(),
    };

    let body = freeze_impl(name, &input.input.data)?;

    let gen = quote! {
        impl #impl_params starlark::values::Freeze for #name #input_params #bounds_body {
            type Frozen = #name #output_params;
            fn freeze(self, freezer: &starlark::values::Freezer) -> anyhow::Result<Self::Frozen> {
                let frozen = #body;
                #validate_body
                std::result::Result::Ok(frozen)
            }
        }
    };

    Ok(gen)
}

#[derive(Default)]
struct FreezeDeriveOptions {
    validator: Option<Ident>,
    bounds: Option<WherePredicate>,
}

/// Parse a #[freeze(validator = function)] annotation.
#[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_impl_dupe))] // The custom_keyword macro
fn extract_options(attrs: &[Attribute]) -> syn::Result<FreezeDeriveOptions> {
    syn::custom_keyword!(validator);
    syn::custom_keyword!(bounds);

    let mut opts = FreezeDeriveOptions::default();

    for attr in attrs.iter() {
        if !attr.path.is_ident("freeze") {
            continue;
        }

        attr.parse_args_with(|input: ParseStream| {
            loop {
                let lookahead = input.lookahead1();
                if lookahead.peek(validator) {
                    if opts.validator.is_some() {
                        return Err(input.error("`validator` was set twice"));
                    }
                    input.parse::<validator>()?;
                    input.parse::<Token![=]>()?;
                    opts.validator = Some(input.parse()?);
                } else if lookahead.peek(bounds) {
                    if opts.bounds.is_some() {
                        return Err(input.error("`bounds` was set twice"));
                    }
                    input.parse::<bounds>()?;
                    input.parse::<Token![=]>()?;
                    let bounds_input = input.parse::<LitStr>()?;
                    opts.bounds = Some(bounds_input.parse()?);
                } else {
                    return Err(lookahead.error());
                }

                if input.parse::<Option<Token![,]>>()?.is_none() {
                    break;
                }
            }

            Ok(())
        })?;
    }

    Ok(opts)
}

/// Parse attribute `#[freeze(identity)]`.
///
/// Currently it fails on any attribute argument other than `id`.
#[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_impl_dupe))] // The custom_keyword macro
fn is_identity(attrs: &[Attribute]) -> syn::Result<bool> {
    syn::custom_keyword!(identity);

    for attr in attrs.iter() {
        if !attr.path.is_ident("freeze") {
            continue;
        }

        let ignore = attr.parse_args_with(|input: ParseStream| {
            let ignore = input.parse::<Option<identity>>()?.is_some();
            Ok(ignore)
        })?;

        if !ignore {
            continue;
        }

        return Ok(true);
    }

    Ok(false)
}

fn freeze_struct(name: &Ident, data: &DataStruct) -> syn::Result<TokenStream> {
    let res = match data.fields {
        Fields::Named(ref fields) => {
            let xs = fields
                .named
                .iter()
                .map(|f| {
                    let name = &f.ident;
                    let res = if is_identity(&f.attrs)? {
                        quote_spanned! {f.span() =>
                            #name: self.#name,
                        }
                    } else {
                        quote_spanned! {f.span() =>
                            #name: starlark::values::Freeze::freeze(self.#name, freezer)?,
                        }
                    };

                    syn::Result::Ok(res)
                })
                .collect::<Result<Vec<_>, _>>()?;
            quote! {
                #name {
                    #(#xs)*
                }
            }
        }
        Fields::Unnamed(ref fields) => {
            let xs = fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    let i = Index::from(i);

                    let res = if is_identity(&f.attrs)? {
                        quote_spanned! {f.span() =>
                            self.#i,
                        }
                    } else {
                        quote_spanned! {f.span() => starlark::values::Freeze::freeze(self.#i, freezer)?,}
                    };

                    syn::Result::Ok(res)
                })
                .collect::<Result<Vec<_>, _>>()?;
            quote! {
                #name (
                    #(#xs)*
                )
            }
        }
        Fields::Unit => {
            quote!()
        }
    };

    Ok(res)
}

fn freeze_enum_variant(name: &Ident, variant: &Variant) -> syn::Result<TokenStream> {
    let variant_name = &variant.ident;
    match &variant.fields {
        Fields::Unit => Ok(quote! {
            #name::#variant_name => #name::#variant_name,
        }),
        Fields::Unnamed(fields) => {
            let field_names: Vec<_> = (0..fields.unnamed.len())
                .map(|i| format_ident!("f_{}", i))
                .collect();
            Ok(quote! {
                #name::#variant_name(#(#field_names),*) => {
                    #name::#variant_name(
                        #(starlark::values::Freeze::freeze(#field_names, freezer)?),*
                    )
                }
            })
        }
        Fields::Named(field) => {
            let field_names: Vec<_> = field.named.iter().map(|f| &f.ident).collect();
            Ok(quote! {
                #name::#variant_name { #(#field_names),* } => {
                    #name::#variant_name {
                        #(#field_names: starlark::values::Freeze::freeze(#field_names, freezer)?,)*
                    }
                }
            })
        }
    }
}

fn freeze_enum(name: &Ident, data: &DataEnum) -> syn::Result<TokenStream> {
    let variants = data
        .variants
        .iter()
        .map(|v| freeze_enum_variant(name, v))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(quote! {
        match self {
            #(#variants)*
        }
    })
}

fn freeze_impl(name: &Ident, data: &Data) -> syn::Result<TokenStream> {
    match data {
        Data::Struct(data) => freeze_struct(name, data),
        Data::Enum(data) => freeze_enum(name, data),
        Data::Union(_) => Err(Error::new_spanned(name, "Can't derive freeze for unions")),
    }
}

pub fn derive_freeze(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match derive_freeze_impl(input) {
        Ok(s) => s.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
