// Copyright (C) 2018-2019 Stephane Raux. Distributed under the MIT license.

//! Procedural macro to derive `IntoEnumIterator` for field-less enums.
//!
//! See crate [enum-iterator](https://docs.rs/enum-iterator) for details.

#![recursion_limit = "128"]
#![deny(warnings)]

extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use std::fmt::{Display, self};
use syn::{Ident, DeriveInput};

/// Derives `IntoEnumIterator` for field-less enums.
#[proc_macro_derive(IntoEnumIterator)]
pub fn into_enum_iterator(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive(input).unwrap_or_else(|e| e.to_compile_error()).into()
}

fn derive(input: proc_macro::TokenStream) -> Result<TokenStream, syn::Error> {
    let ast = syn::parse::<DeriveInput>(input)?;
    if !ast.generics.params.is_empty() {
        return Err(Error::GenericsUnsupported.with_tokens(&ast.generics));
    }
    let ty = &ast.ident;
    let vis = &ast.vis;
    let ty_doc = format!("Iterator over the variants of {}", ty);
    let iter_ty = Ident::new(&(ty.to_string() + "EnumIterator"), Span::call_site());
    let variants = match &ast.data {
        syn::Data::Enum(e) => &e.variants,
        _ => return Err(Error::ExpectedEnum.with_tokens(&ast)),
    };
    let arms = variants.iter().enumerate()
        .map(|(idx, v)| {
            let id = &v.ident;
            match v.fields {
                syn::Fields::Unit => Ok(quote! { #idx => #ty::#id, }),
                _ => Err(Error::ExpectedUnitVariant.with_tokens(v)),
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    let nb_variants = arms.len();
    let tokens = quote! {
        #[doc = #ty_doc]
        #[derive(Clone, Copy, Debug)]
        #vis struct #iter_ty {
            idx: usize,
        }

        impl ::std::iter::Iterator for #iter_ty {
            type Item = #ty;

            fn next(&mut self) -> ::std::option::Option<Self::Item> {
                let id = match self.idx {
                    #(#arms)*
                    _ => return ::std::option::Option::None,
                };
                self.idx += 1;
                ::std::option::Option::Some(id)
            }

            fn size_hint(&self) -> (usize, ::std::option::Option<usize>) {
                let n = #nb_variants - self.idx;
                (n, ::std::option::Option::Some(n))
            }
        }

        impl ::std::iter::ExactSizeIterator for #iter_ty {}
        impl ::std::iter::FusedIterator for #iter_ty {}

        impl ::enum_iterator::IntoEnumIterator for #ty {
            type Iterator = #iter_ty;

            const VARIANT_COUNT: usize = #nb_variants;

            fn into_enum_iter() -> Self::Iterator {
                #iter_ty { idx: 0 }
            }
        }
    };
    let tokens = quote! {
        const _: () = {
            #tokens
        };
    };
    Ok(tokens)
}

#[derive(Debug)]
enum Error {
    ExpectedEnum,
    ExpectedUnitVariant,
    GenericsUnsupported,
}

impl Error {
    fn with_tokens<T: ToTokens>(self, tokens: T) -> syn::Error {
        syn::Error::new_spanned(tokens, self)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ExpectedEnum =>
                f.write_str("IntoEnumIterator can only be derived for enum types"),
            Error::ExpectedUnitVariant =>
                f.write_str("IntoEnumIterator can only be derived for enum types with unit \
                    variants only"),
            Error::GenericsUnsupported =>
                f.write_str("IntoEnumIterator cannot be derived for generic types"),
        }
    }
}
