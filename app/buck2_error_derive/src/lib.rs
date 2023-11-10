/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(proc_macro_def_site)]
#![feature(extract_if)]
#![feature(iterator_try_collect)]

use proc_macro2::Span;
use proc_macro2::TokenStream;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::parse::Parser;
use syn::parse_macro_input;
use syn::parse_quote;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::Token;

/// Did the user provide an explicit value for the option, or a function from which to compute it
#[derive(Clone)]
enum OptionStyle {
    Explicit(syn::Ident),
    ByFunc(syn::Path),
}

impl OptionStyle {
    fn span(&self) -> Span {
        match self {
            Self::Explicit(ident) => ident.span(),
            Self::ByFunc(path) => path.span(),
        }
    }
}

impl Parse for OptionStyle {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let path: syn::Path = input.parse()?;

        if input.is_empty() {
            let ident = path.require_ident()?;
            Ok(Self::Explicit(ident.clone()))
        } else {
            let inner;
            syn::parenthesized!(inner in input);
            let _underscore: Token![_] = inner.parse()?;
            Ok(Self::ByFunc(path))
        }
    }
}

enum MacroOption {
    Category(OptionStyle),
    Typ(OptionStyle),
}

impl Parse for MacroOption {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        if name == "user" {
            let ident = syn::Ident::new("User", name.span());
            Ok(MacroOption::Category(OptionStyle::Explicit(ident)))
        } else if name == "infra" {
            let ident = syn::Ident::new("Infra", name.span());
            Ok(MacroOption::Category(OptionStyle::Explicit(ident)))
        } else if name == "category" {
            let _eq: Token![=] = input.parse()?;
            Ok(MacroOption::Category(input.parse()?))
        } else if name == "typ" {
            let _eq: Token![=] = input.parse()?;
            Ok(MacroOption::Typ(input.parse()?))
        } else {
            Err(syn::Error::new_spanned(name, "expected option"))
        }
    }
}

#[derive(Default, Clone)]
struct ParsedOptions {
    category: Option<OptionStyle>,
    typ: Option<OptionStyle>,
}

/// Indicates that an option was provided which is only allowed on error roots, and is therefore
/// incompatible with the `#[source]` attribute.
fn is_incompatible_with_source(p: &ParsedOptions) -> bool {
    p.typ.is_some()
}

fn parse_attributes(
    attrs: &mut Vec<syn::Attribute>,
    fields: &syn::Fields,
    parsed_earlier: Option<&ParsedOptions>,
) -> syn::Result<Option<ParsedOptions>> {
    let mut all_options = Vec::new();

    for attr in attrs.extract_if(|attr| attr.meta.path().get_ident().is_some_and(|i| i == "buck2"))
    {
        if parsed_earlier.is_some() {
            return Err(syn::Error::new_spanned(
                attr,
                "Cannot specify options on both the type and the variant",
            ));
        }
        let meta = attr.meta.require_list()?;
        let parsed =
            Punctuated::<MacroOption, Token![,]>::parse_terminated.parse2(meta.tokens.clone())?;
        all_options.extend(parsed);
    }

    if all_options.is_empty() {
        return Ok(None);
    }

    let mut parsed_options = ParsedOptions {
        category: None,
        typ: None,
    };

    for option in all_options {
        match option {
            MacroOption::Category(style) => {
                if parsed_options.category.is_some() {
                    return Err(syn::Error::new(style.span(), "duplicate category"));
                }
                parsed_options.category = Some(style);
            }
            MacroOption::Typ(style) => {
                if parsed_options.typ.is_some() {
                    return Err(syn::Error::new(style.span(), "duplicate error type"));
                }
                parsed_options.typ = Some(style);
            }
        }
    }

    for attr in fields.iter().flat_map(|f| f.attrs.iter()) {
        let ident = attr.meta.path().get_ident();
        if parsed_earlier.is_some_and(is_incompatible_with_source)
            || is_incompatible_with_source(&parsed_options)
        {
            if ident.is_some_and(|i| i == "source" || i == "from") {
                return Err(syn::Error::new_spanned(
                    attr,
                    "cannot be used with root-only buck2 options",
                ));
            }
        }

        if ident.is_some_and(|i| i == "buck2") {
            return Err(syn::Error::new_spanned(
                attr,
                "put this on the type or variant, not a field",
            ));
        }
    }

    Ok(Some(parsed_options))
}

fn render_options(
    options: Option<ParsedOptions>,
    krate: &syn::Path,
    type_and_variant: &str,
) -> proc_macro2::TokenStream {
    let options = options.unwrap_or_default();
    let source_location_extra = syn::LitStr::new(type_and_variant, Span::call_site());
    let category = options.category.map(|cat| match cat {
        OptionStyle::Explicit(cat) => quote::quote! {
            category = ::core::option::Option::Some(#krate::Category::#cat);
        },
        OptionStyle::ByFunc(func) => quote::quote! {
            category = #func(&val);
        },
    });
    let typ = options.typ.map(|typ| match typ {
        OptionStyle::Explicit(typ) => quote::quote! {
            typ = ::core::option::Option::Some(#krate::ErrorType::#typ);
        },
        OptionStyle::ByFunc(func) => quote::quote! {
            typ = #func(&val);
        },
    });

    quote::quote! {
        source_location_extra = #source_location_extra;
        #category
        #typ
    }
}

fn fields_as_pat(fields: &syn::Fields) -> proc_macro2::TokenStream {
    match fields {
        syn::Fields::Named(fields) => {
            // Because we always access the fields of the outer type by transmute, Rust thinks
            // they're unused and issues a warning. Insert a dummy use here to silence it.
            let fields = fields.named.iter().map(|field| {
                let name = field.ident.as_ref().unwrap();
                let underscore_name = syn::Ident::new(&format!("_{}", name), name.span());
                quote::quote! {
                    #name: #underscore_name
                }
            });
            quote::quote! {
                { #(#fields ,)* }
            }
        }
        syn::Fields::Unnamed(_) => quote::quote! {
            (..)
        },
        syn::Fields::Unit => quote::quote! {},
    }
}

/// Generates appropriate assignments to `category` and `typ` for the value in `val`.
///
/// Also, removes any attributes specific to this macro from the input.
fn generate_option_assignments(
    input: &mut syn::DeriveInput,
    krate: &syn::Path,
) -> syn::Result<proc_macro2::TokenStream> {
    let name = input.ident.clone();
    match &mut input.data {
        syn::Data::Struct(data) => {
            let options = parse_attributes(&mut input.attrs, &data.fields, None)?;
            let options = render_options(options, krate, &name.to_string());
            let fields_as_pat = fields_as_pat(&data.fields);
            Ok(quote::quote! {
                match val { #name #fields_as_pat => (), };
                #options
            })
        }
        syn::Data::Enum(data) => {
            let type_options = parse_attributes(&mut input.attrs, &syn::Fields::Unit, None)?;
            // `type_and_variant` does not matter, as it's overwritten in each variant later anyway
            let type_options_rendered = render_options(type_options.clone(), krate, "");
            let variants = data
                .variants
                .iter_mut()
                .map(|variant| {
                    let type_and_variant = format!("{}::{}", name, variant.ident);
                    let options = parse_attributes(
                        &mut variant.attrs,
                        &variant.fields,
                        type_options.as_ref(),
                    )?;
                    let rendered_options = render_options(options, krate, &type_and_variant);
                    let fields_as_pat = fields_as_pat(&variant.fields);
                    let variant_name = &variant.ident;
                    Ok::<_, syn::Error>(quote::quote! {
                        #name::#variant_name #fields_as_pat => { #rendered_options },
                    })
                })
                .try_collect::<Vec<_>>()?;

            Ok(quote::quote! {
                #type_options_rendered
                match val { #(#variants)* };
            })
        }
        syn::Data::Union(_) => Err(syn::Error::new_spanned(input, "Unions are not supported")),
    }
}

fn derive_error_impl(mut input: syn::DeriveInput, krate: syn::Path) -> syn::Result<TokenStream> {
    let def_site: proc_macro2::Span = proc_macro::Span::def_site().into();

    let option_assignments = generate_option_assignments(&mut input, &krate)?;
    let (_, type_generics, where_clauses) = input.generics.split_for_impl();
    let mut impl_generics = input.generics.params.clone();
    if !impl_generics.empty_or_trailing() {
        impl_generics.push_punct(Default::default());
    }

    // In order to make this macro work, we have to do a number of cursed things with reexports. As
    // a result, we need to be careful about not polluting the namespace in which the macro is
    // invoked. We achieve this by putting everything generated by the macro into a function. This
    // ident is the name of that function.
    let hidden_function_name = syn::Ident::new(
        &format!("__macro_generated_by_buck2_error_hidden_{}", input.ident),
        input.ident.span().resolved_at(def_site),
    );
    // Our goal is effectively to "hijack" the impls created by `thiserror::Error`. However, without
    // eager macro expansion this is not so easy to do. The strategy we opt for is to have two
    // types, an outer and an inner one. The outer type is the one that this derive macro is invoked
    // on. The inner one is a second type with the same definition - the `thiserror::Error` impl
    // will be applied to the inner type. The outer type will explicitly impl a number of traits,
    // often forwarding to the inner.
    //
    // Ideally we would implement this by using def site hygiene, instead of changing the name of
    // the type. However, that doesn't work because of a rustc bug with hygiened inheritance for
    // derive macros. See a previous attempt in 9b1d890e45dc.
    let outer = &input.ident;
    let mut input_for_inner = input.clone();
    input_for_inner.ident = syn::Ident::new(&format!("{}_", outer), outer.span());
    let inner = &input_for_inner.ident;

    // In order to forward from the outer type to the inner, we will need to transmute between them.
    // In the case of by-value conversions this would be avoidable, but in the case of reference
    // conversions it's not. The correctness of this relies on the assumption that type layout in
    // Rust is a function of the type definition only, ie that types with identical definition have
    // identical layout. This is true today, but is not guaranteed. If it ever changes, we will need
    // to start requiring `#[repr(C)]` or find a different way to implement this.
    let lifetime = quote::quote! {
        'generated_lifetime_name
    };
    let ref_transmute = quote::quote! {
        ::core::mem::transmute::<&#lifetime #outer #type_generics, &#lifetime #inner #type_generics>
    };

    let derive_attribute = quote::quote! {
        #[derive(::core::fmt::Debug, thiserror::Error)]
    };

    let mut where_clauses = match where_clauses {
        Some(x) => x.predicates.clone(),
        None => Punctuated::new(),
    };
    if !where_clauses.empty_or_trailing() {
        where_clauses.push_punct(Default::default());
    }

    Ok(quote::quote! {
        #[allow(unused)]
        #[doc(hidden)]
        #[allow(non_snake_case)]
        fn #hidden_function_name() {
            use #krate::__for_macro::thiserror;

            #derive_attribute
            #input_for_inner

            impl < #impl_generics > ::std::error::Error for #outer #type_generics
            where #where_clauses
            Self: ::core::fmt::Debug + ::core::fmt::Display + ::core::marker::Send + ::core::marker::Sync + 'static,
            #inner #type_generics : ::std::error::Error
            {
                fn source<#lifetime>(&#lifetime self) -> Option<&(dyn ::std::error::Error + 'static)> {
                    let val = unsafe { #ref_transmute(self) };
                    <_ as ::std::error::Error>::source(val)
                }

                fn description<#lifetime>(&#lifetime self) -> &str {
                    let val = unsafe { #ref_transmute(self) };
                    <_ as ::std::error::Error>::description(val)
                }

                fn cause<#lifetime>(&#lifetime self) -> Option<&dyn ::std::error::Error> {
                    let val = unsafe { #ref_transmute(self) };
                    <_ as ::std::error::Error>::cause(val)
                }

                fn provide<#lifetime>(&#lifetime self, demand: &mut #krate::Demand<#lifetime>) {
                    let val = self;
                    #[allow(unused_mut)]
                    let mut typ = ::core::option::Option::None;
                    #[allow(unused_mut)]
                    let mut category = ::core::option::Option::None;
                    #[allow(unused_mut)]
                    let mut source_location_extra;
                    #option_assignments
                    #krate::provide_metadata::<Self>(
                        demand,
                        category,
                        typ,
                        ::core::file!(),
                        ::core::option::Option::Some(source_location_extra),
                    );
                    let val = unsafe { #ref_transmute(self) };
                    <_ as ::std::error::Error>::provide(val, demand)
                }
            }

            impl < #impl_generics > ::core::fmt::Display for #outer #type_generics
            where #where_clauses
            #inner #type_generics : ::core::fmt::Display
            {
                fn fmt<#lifetime>(&#lifetime self, f: &mut ::core::fmt::Formatter<'_>)
                -> ::core::fmt::Result {
                    let val = unsafe { #ref_transmute(self) };
                    ::core::fmt::Display::fmt(val, f)
                }
            }

            impl < #impl_generics __FOR_MACRO_T> ::core::convert::From<__FOR_MACRO_T> for #outer #type_generics
            where #inner #type_generics : ::core::convert::From<__FOR_MACRO_T>
            {
                fn from(val: __FOR_MACRO_T) -> Self {
                    let val: #inner #type_generics = ::core::convert::From::from(val);
                    let out = unsafe { ::core::ptr::read(&val as *const _ as *const _) };
                    #[allow(clippy::forget_non_drop)]
                    ::core::mem::forget(val);
                    out
                }
            }
        }
    })
}

#[proc_macro_derive(ErrorForReexport, attributes(backtrace, error, from, source, buck2))]
pub fn derive_error_for_reexport(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    match derive_error_impl(input, parse_quote! { ::buck2_error }) {
        Ok(x) => x.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

#[proc_macro_derive(Error, attributes(backtrace, error, from, source, buck2))]
pub fn derive_error(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    match derive_error_impl(input, parse_quote! { crate }) {
        Ok(x) => x.into(),
        Err(e) => e.into_compile_error().into(),
    }
}
