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

use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote_spanned;
use syn::parse::ParseStream;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::Attribute;
use syn::Data;
use syn::DataEnum;
use syn::DataStruct;
use syn::DeriveInput;
use syn::Error;
use syn::Fields;
use syn::GenericParam;
use syn::Index;
use syn::LitStr;
use syn::Token;
use syn::Variant;
use syn::WherePredicate;

struct Input<'a> {
    input: &'a DeriveInput,
}

impl<'a> Input<'a> {
    fn angle_brankets(&self, tokens: &[TokenStream]) -> TokenStream {
        let span = self.input.span();
        if tokens.is_empty() {
            quote_spanned! { span=> }
        } else {
            quote_spanned! { span=> < #(#tokens,)* > }
        }
    }

    fn format_impl_generics(
        &self,
        bounds: bool,
    ) -> syn::Result<(TokenStream, TokenStream, TokenStream)> {
        let span = self.input.span();
        let mut impl_params = Vec::new();
        let mut input_params = Vec::new();
        let mut output_params = Vec::new();
        if bounds {
            impl_params.push(quote_spanned! { span=> 'freeze });
        }
        for param in &self.input.generics.params {
            match param {
                GenericParam::Type(t) => {
                    let name = &t.ident;
                    let bounds = t.bounds.iter();
                    impl_params.push(quote_spanned! {
                        span=>
                        #name: #(#bounds +)* starlark::values::Freeze
                    });
                    input_params.push(quote_spanned! {
                        span=>
                        #name
                    });
                    output_params.push(quote_spanned! {
                        span=>
                        <#name as starlark::values::Freeze>::Frozen
                    });
                }
                GenericParam::Lifetime(lt) => {
                    impl_params.push(quote_spanned! { span=> #lt });
                    input_params.push(quote_spanned! { span=> #lt });
                    output_params.push(quote_spanned! { span=> 'static });
                }
                GenericParam::Const(_) => {
                    return Err(Error::new_spanned(param, "const generics not supported"));
                }
            }
        }
        Ok((
            self.angle_brankets(&impl_params),
            self.angle_brankets(&input_params),
            self.angle_brankets(&output_params),
        ))
    }
}

fn derive_freeze_impl(input: DeriveInput) -> syn::Result<TokenStream> {
    let span = input.span();
    let input = Input { input: &input };

    let name = &input.input.ident;

    let opts = extract_options(&input.input.attrs)?;
    let (impl_params, input_params, output_params) =
        input.format_impl_generics(opts.bounds.is_some())?;

    let validate_body = match opts.validator {
        Some(validator) => quote_spanned! {
            span=>
            #validator(&frozen)?;
        },
        None => quote_spanned! { span=> },
    };

    let bounds_body = match opts.bounds {
        Some(bounds) => quote_spanned! { span=> where #bounds },
        None => quote_spanned! { span=> },
    };

    let body = freeze_impl(name, &input.input.data)?;

    let gen = quote_spanned! {
        span=>
        impl #impl_params starlark::values::Freeze for #name #input_params #bounds_body {
            type Frozen = #name #output_params;
            #[allow(unused_variables)]
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
                if input.parse::<validator>().is_ok() {
                    if opts.validator.is_some() {
                        return Err(input.error("`validator` was set twice"));
                    }
                    input.parse::<Token![=]>()?;
                    opts.validator = Some(input.parse()?);
                } else if input.parse::<bounds>().is_ok() {
                    if opts.bounds.is_some() {
                        return Err(input.error("`bounds` was set twice"));
                    }
                    input.parse::<Token![=]>()?;
                    let bounds_input = input.parse::<LitStr>()?;
                    opts.bounds = Some(bounds_input.parse()?);
                } else {
                    return Err(input.lookahead1().error());
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
    let span = name.span();
    let res = match data.fields {
        Fields::Named(ref fields) => {
            let xs = fields
                .named
                .iter()
                .map(|f| {
                    let name = &f.ident;
                    let res = if is_identity(&f.attrs)? {
                        quote_spanned! { span=>
                            #name: self.#name,
                        }
                    } else {
                        quote_spanned! { span=>
                            #name: starlark::values::Freeze::freeze(self.#name, freezer)?,
                        }
                    };

                    syn::Result::Ok(res)
                })
                .collect::<Result<Vec<_>, _>>()?;
            quote_spanned! {
                span=>
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
                        quote_spanned! { span=>
                            self.#i,
                        }
                    } else {
                        quote_spanned! {
                            span=>
                            starlark::values::Freeze::freeze(self.#i, freezer)?,
                        }
                    };

                    syn::Result::Ok(res)
                })
                .collect::<Result<Vec<_>, _>>()?;
            quote_spanned! {
                span=>
                #name (
                    #(#xs)*
                )
            }
        }
        Fields::Unit => {
            quote_spanned! { span=> }
        }
    };

    Ok(res)
}

fn freeze_enum_variant(name: &Ident, variant: &Variant) -> syn::Result<TokenStream> {
    let span = variant.span();
    let variant_name = &variant.ident;
    match &variant.fields {
        Fields::Unit => Ok(quote_spanned! {
            span=>
            #name::#variant_name => #name::#variant_name,
        }),
        Fields::Unnamed(fields) => {
            let field_names: Vec<_> = (0..fields.unnamed.len())
                .map(|i| format_ident!("f_{}", i))
                .collect();
            Ok(quote_spanned! {
                span=>
                #name::#variant_name(#(#field_names),*) => {
                    #name::#variant_name(
                        #(starlark::values::Freeze::freeze(#field_names, freezer)?),*
                    )
                }
            })
        }
        Fields::Named(field) => {
            let field_names: Vec<_> = field.named.iter().map(|f| &f.ident).collect();
            Ok(quote_spanned! {
                span=>
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
    let span = name.span();
    let variants = data
        .variants
        .iter()
        .map(|v| freeze_enum_variant(name, v))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(quote_spanned! {
        span=>
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
