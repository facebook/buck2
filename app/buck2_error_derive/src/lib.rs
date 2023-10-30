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

use proc_macro::TokenStream;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::parse::Parser;
use syn::parse_macro_input;
use syn::parse_quote;
use syn::punctuated::Punctuated;
use syn::Token;

enum MacroOption {
    User(syn::Ident),
    Infra(syn::Ident),
}

impl Parse for MacroOption {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        if name == "user" {
            Ok(MacroOption::User(name))
        } else if name == "infra" {
            Ok(MacroOption::Infra(name))
        } else {
            Err(syn::Error::new_spanned(name, "expected option"))
        }
    }
}

struct ParsedOptions {
    category: Option<syn::Ident>,
}

fn parse_attributes(
    attrs: &mut Vec<syn::Attribute>,
    always_fail: Option<&str>,
) -> syn::Result<ParsedOptions> {
    let mut all_options = Vec::new();

    for attr in attrs.extract_if(|attr| attr.meta.path().get_ident().is_some_and(|i| i == "buck2"))
    {
        if let Some(message) = always_fail {
            return Err(syn::Error::new_spanned(attr, message));
        }
        let meta = attr.meta.require_list()?;
        let parsed =
            Punctuated::<MacroOption, Token![,]>::parse_terminated.parse2(meta.tokens.clone())?;
        all_options.extend(parsed);
    }

    let mut parsed_options = ParsedOptions { category: None };

    for option in all_options {
        match option {
            MacroOption::User(ident) => {
                if parsed_options.category.is_some() {
                    return Err(syn::Error::new_spanned(ident, "duplicate category"));
                }
                parsed_options.category = Some(syn::Ident::new("User", ident.span()));
            }
            MacroOption::Infra(ident) => {
                if parsed_options.category.is_some() {
                    return Err(syn::Error::new_spanned(ident, "duplicate category"));
                }
                parsed_options.category = Some(syn::Ident::new("Infra", ident.span()));
            }
        }
    }

    Ok(parsed_options)
}

fn render_options(options: ParsedOptions, krate: &syn::Path) -> proc_macro2::TokenStream {
    let category = options.category.map(|cat| {
        quote::quote! {
            category = ::core::option::Option::Some(#krate::Category::#cat);
        }
    });

    quote::quote! {
        #category
    }
}

fn fields_as_pat(fields: &syn::Fields) -> proc_macro2::TokenStream {
    match fields {
        syn::Fields::Named(_) => quote::quote! {
            { .. }
        },
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
            for field in &mut data.fields {
                parse_attributes(
                    &mut field.attrs,
                    Some("Attribute must be on the type, not a field"),
                )?;
            }
            let options = parse_attributes(&mut input.attrs, None)?;
            Ok(render_options(options, krate))
        }
        syn::Data::Enum(data) => {
            let variants = data
                .variants
                .iter_mut()
                .map(|variant| {
                    let options = parse_attributes(&mut variant.attrs, None)?;
                    let rendered_options = render_options(options, krate);
                    let fields_as_pat = fields_as_pat(&variant.fields);
                    let variant_name = &variant.ident;
                    Ok::<_, syn::Error>(quote::quote! {
                        #name::#variant_name #fields_as_pat => { #rendered_options },
                    })
                })
                .try_collect::<Vec<_>>()?;

            parse_attributes(
                &mut input.attrs,
                Some("Attribute must be on the variant, not the type"),
            )?;

            Ok(quote::quote! {
                match val { #(#variants)* };
            })
        }
        syn::Data::Union(_) => Err(syn::Error::new_spanned(input, "Unions are not supported")),
    }
}

fn derive_error_impl(input: TokenStream, krate: syn::Path) -> TokenStream {
    let def_site: proc_macro2::Span = proc_macro::Span::def_site().into();

    // For now, this is more or less just a stub that forwards to `thiserror`. It doesn't yet do
    // anything interesting.
    let mut input = parse_macro_input!(input as syn::DeriveInput);
    let option_assignments = match generate_option_assignments(&mut input, &krate) {
        Ok(x) => x,
        Err(e) => return e.into_compile_error().into(),
    };
    let (impl_generics, type_generics, where_clauses) = input.generics.split_for_impl();

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
    let val_transmute = quote::quote! {
        ::core::mem::transmute::<#outer, #inner>
    };
    let lifetime = quote::quote! {
        'generated_lifetime_name
    };
    let ref_transmute = quote::quote! {
        ::core::mem::transmute::<&#lifetime #outer, &#lifetime #inner>
    };

    let derive_attribute = quote::quote! {
        #[derive(::core::fmt::Debug, thiserror::Error)]
    };
    quote::quote! {
        #[allow(unused)]
        #[doc(hidden)]
        #[allow(non_snake_case)]
        fn #hidden_function_name() {
            use #krate::__for_macro::thiserror;

            #derive_attribute
            #input_for_inner

            impl #impl_generics ::core::convert::From<#outer #type_generics> for #krate::Error
            #where_clauses
            {
                fn from(val: #outer #type_generics) -> Self {
                    #[allow(unused_mut)]
                    let mut typ = ::core::option::Option::None;
                    #[allow(unused_mut)]
                    let mut category = ::core::option::Option::None;
                    #option_assignments
                    let val: #inner = unsafe { #val_transmute(val) };
                    #krate::__for_macro::new_with_macro_options(val, typ, category)
                }
            }
            impl #impl_generics #krate::AnyError for #outer #type_generics
            #where_clauses {}
            impl #impl_generics #krate::__for_macro::Sealed for #outer #type_generics
            #where_clauses {}

            impl #impl_generics ::core::convert::From<#outer #type_generics>
            for #krate::__for_macro::anyhow::Error
            #where_clauses
            {
                fn from(val: #outer #type_generics) -> Self {
                    let val: #krate::Error = ::core::convert::From::from(val);
                    ::core::convert::Into::into(val)
                }
            }

            impl #impl_generics ::core::fmt::Display for #outer #type_generics #where_clauses
            {
                fn fmt<#lifetime>(&#lifetime self, f: &mut ::core::fmt::Formatter<'_>)
                -> ::core::fmt::Result {
                    let val = unsafe { #ref_transmute(self) };
                    ::core::fmt::Display::fmt(val, f)
                }
            }
        }
    }
    .into()
}

#[proc_macro_derive(ErrorForReexport, attributes(backtrace, error, from, source, buck2))]
pub fn derive_error_for_reexport(input: TokenStream) -> TokenStream {
    derive_error_impl(input, parse_quote! { ::buck2_error })
}

#[proc_macro_derive(Error, attributes(backtrace, error, from, source, buck2))]
pub fn derive_error(input: TokenStream) -> TokenStream {
    derive_error_impl(input, parse_quote! { crate })
}

// Implementation detail of `derive_error`
#[doc(hidden)]
#[proc_macro_attribute]
pub fn exterminate(_attr: TokenStream, _input: TokenStream) -> TokenStream {
    TokenStream::default()
}
