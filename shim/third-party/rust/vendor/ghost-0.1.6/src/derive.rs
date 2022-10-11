use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Parser};
use syn::punctuated::Punctuated;
use syn::{parenthesized, Attribute, Error, Path, Result, Token};

use crate::parse::UnitStruct;

enum Derive {
    Copy,
    Clone,
    Default,
    Hash,
    PartialOrd,
    Ord,
    PartialEq,
    Eq,
    Debug,
}

struct DeriveList {
    derives: Vec<Derive>,
}

impl Parse for DeriveList {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        parenthesized!(content in input);
        let paths: Punctuated<Path, Token![,]> = content.parse_terminated(Path::parse_mod_style)?;

        let mut derives = Vec::new();
        for path in paths {
            if path.is_ident("Copy") {
                derives.push(Derive::Copy);
            } else if path.is_ident("Clone") {
                derives.push(Derive::Clone);
            } else if path.is_ident("Default") {
                derives.push(Derive::Default);
            } else if path.is_ident("Hash") {
                derives.push(Derive::Hash);
            } else if path.is_ident("PartialOrd") {
                derives.push(Derive::PartialOrd);
            } else if path.is_ident("Ord") {
                derives.push(Derive::Ord);
            } else if path.is_ident("PartialEq") {
                derives.push(Derive::PartialEq);
            } else if path.is_ident("Eq") {
                derives.push(Derive::Eq);
            } else if path.is_ident("Debug") {
                derives.push(Derive::Debug);
            } else {
                return Err(Error::new_spanned(path, "unsupported derive"));
            }
        }

        Ok(DeriveList { derives })
    }
}

pub fn expand<'a>(
    attrs: &'a [Attribute],
    input: &UnitStruct,
) -> Result<(TokenStream, Vec<&'a Attribute>)> {
    let mut expanded = TokenStream::new();
    let mut non_derives = Vec::new();

    for attr in attrs {
        if attr.path.is_ident("derive") {
            let list = DeriveList::parse.parse2(attr.tokens.clone())?;
            for derive in list.derives {
                expanded.extend(apply(derive, input));
            }
        } else {
            non_derives.push(attr);
        }
    }

    Ok((expanded, non_derives))
}

fn apply(derive: Derive, input: &UnitStruct) -> TokenStream {
    match derive {
        Derive::Copy => expand_copy(input),
        Derive::Clone => expand_clone(input),
        Derive::Default => expand_default(input),
        Derive::Hash => expand_hash(input),
        Derive::PartialOrd => expand_partialord(input),
        Derive::Ord => expand_ord(input),
        Derive::PartialEq => expand_partialeq(input),
        Derive::Eq => expand_eq(input),
        Derive::Debug => expand_debug(input),
    }
}

fn expand_copy(input: &UnitStruct) -> TokenStream {
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote! {
        impl #impl_generics ::core::marker::Copy
        for #ident #ty_generics #where_clause {}
    }
}

fn expand_clone(input: &UnitStruct) -> TokenStream {
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote! {
        #[allow(clippy::expl_impl_clone_on_copy)]
        impl #impl_generics ::core::clone::Clone
        for #ident #ty_generics #where_clause {
            #[inline]
            fn clone(&self) -> Self {
                #ident
            }
        }
    }
}

fn expand_default(input: &UnitStruct) -> TokenStream {
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote! {
        impl #impl_generics ::core::default::Default
        for #ident #ty_generics #where_clause {
            #[inline]
            fn default() -> Self {
                #ident
            }
        }
    }
}

fn expand_hash(input: &UnitStruct) -> TokenStream {
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote! {
        impl #impl_generics ::core::hash::Hash
        for #ident #ty_generics #where_clause {
            #[inline]
            fn hash<H: ::core::hash::Hasher>(&self, hasher: &mut H) {
                let _ = hasher;
            }
        }
    }
}

fn expand_partialord(input: &UnitStruct) -> TokenStream {
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote! {
        impl #impl_generics ::core::cmp::PartialOrd
        for #ident #ty_generics #where_clause {
            #[inline]
            fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering> {
                let _ = other;
                ::core::option::Option::Some(::core::cmp::Ordering::Equal)
            }
        }
    }
}

fn expand_ord(input: &UnitStruct) -> TokenStream {
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote! {
        impl #impl_generics ::core::cmp::Ord
        for #ident #ty_generics #where_clause {
            #[inline]
            fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
                let _ = other;
                ::core::cmp::Ordering::Equal
            }
        }
    }
}

fn expand_partialeq(input: &UnitStruct) -> TokenStream {
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote! {
        impl #impl_generics ::core::cmp::PartialEq
        for #ident #ty_generics #where_clause {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                let _ = other;
                true
            }
        }
    }
}

fn expand_eq(input: &UnitStruct) -> TokenStream {
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote! {
        impl #impl_generics ::core::cmp::Eq
        for #ident #ty_generics #where_clause {}
    }
}

fn expand_debug(input: &UnitStruct) -> TokenStream {
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let string = ident.to_string();

    quote! {
        impl #impl_generics ::core::fmt::Debug
        for #ident #ty_generics #where_clause {
            fn fmt(&self, formatter: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::write_str(formatter, #string)
            }
        }
    }
}
