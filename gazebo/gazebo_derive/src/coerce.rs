/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;

use proc_macro2::Span;
use proc_macro2::TokenTree;
use quote::format_ident;
use quote::quote;
use syn::parse::ParseStream;
use syn::parse_macro_input;
use syn::token;
use syn::Data;
use syn::DeriveInput;
use syn::Error;
use syn::Expr;
use syn::GenericArgument;
use syn::Ident;
use syn::Path;
use syn::PathArguments;
use syn::ReturnType;
use syn::Token;
use syn::Type;

// This macro does two related derivations depending on whether there are any generic parameters.
//
// struct A(B) ==> coerce both ways between A and B
// struct A<T>(...) => coerce A<T1> to A<T2> if coerce T1 to T2 and all the fields support it
pub fn derive_coerce(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    if let Err(err) = check_repr(&input) {
        return err.into_compile_error().into();
    }

    let lifetimes = input.generics.lifetimes().collect::<Vec<_>>();

    if input.generics.type_params().count() == 0 {
        let field = match &input.data {
            Data::Struct(x) if x.fields.len() == 1 => x.fields.iter().next().unwrap(),
            _ => {
                return syn::Error::new_spanned(
                    input,
                    "Type-parameter free types must be a single field struct",
                )
                .into_compile_error()
                .into();
            }
        };

        let type1 = input.ident;
        let type2 = &field.ty;
        let gen = quote! {
            unsafe impl < #(#lifetimes),* > gazebo::coerce::Coerce<#type1< #(#lifetimes),* >> for #type2 {}
            unsafe impl < #(#lifetimes),* > gazebo::coerce::Coerce<#type2> for #type1< #(#lifetimes),* > {}
        };
        gen.into()
    } else {
        // unsafe impl <To__T, From__T> Coerce<X<To__T>> X<From__T> where ...
        let name = &input.ident;
        let mut ty_args = HashSet::new();
        let mut ty_args_to = Vec::new();
        let mut ty_args_from = Vec::new();
        let mut ty_args_to1 = Vec::new();
        let mut ty_args_from1 = Vec::new();
        for t in input.generics.type_params() {
            ty_args.insert(t.ident.clone());
            let to_ident = format_ident!("To{}", t.ident);
            let from_ident = format_ident!("From{}", t.ident);
            ty_args_to.push(to_ident.clone());
            ty_args_from.push(from_ident.clone());
            if t.bounds.is_empty() {
                ty_args_to1.push(quote! { #to_ident });
                ty_args_from1.push(quote! { #from_ident });
            } else {
                let bounds = &t.bounds;
                ty_args_to1.push(quote! { #to_ident: #bounds });
                ty_args_from1.push(quote! { #from_ident: #bounds });
            }
        }

        let mut constraints = Vec::new();
        let fields: Vec<_> = match &input.data {
            Data::Struct(x) => x.fields.iter().collect(),
            Data::Enum(x) => x.variants.iter().flat_map(|x| &x.fields).collect(),
            _ => {
                return syn::Error::new_spanned(input, "Type-parameter cannot be a union")
                    .into_compile_error()
                    .into();
            }
        };
        for x in fields {
            let mut to_ty = x.ty.clone();
            let mut from_ty = x.ty.clone();
            let to = replace_type(&mut to_ty, &ty_args, "To");
            let from = replace_type(&mut from_ty, &ty_args, "From");
            if to.is_none() || from.is_none() {
                return syn::Error::new_spanned(
                    &input,
                    "Don't know how to deal with some of the fields",
                )
                .into_compile_error()
                .into();
            }
            if to_ty != from_ty {
                constraints.push(quote! { #from_ty : gazebo::coerce::Coerce< #to_ty >});
            }
        }

        let gen = quote! {
            unsafe impl < #(#lifetimes,)* #(#ty_args_from1,)* #(#ty_args_to1,)* >
                gazebo::coerce::Coerce<#name < #(#lifetimes,)* #(#ty_args_to,)* >>
                for #name < #(#lifetimes,)* #(#ty_args_from,)* >
                where #(#constraints,)* {}
        };
        gen.into()
    }
}

fn replace_type(ty: &mut Type, idents: &HashSet<Ident>, prefix: &str) -> Option<()> {
    match ty {
        Type::Path(x)
            if x.qself.is_none()
                && x.path.segments.len() == 1
                && x.path.segments[0].arguments.is_empty() =>
        {
            let i = &mut x.path.segments[0].ident;
            if idents.contains(i) {
                *i = format_ident!("{}{}", prefix, i);
            }
            Some(())
        }
        _ => descend_type(ty, |ty| replace_type(ty, idents, prefix)),
    }
}

/// Descend into all the nested type values within a type, or return None if you don't know how
fn descend_type(ty: &mut Type, op: impl Fn(&mut Type) -> Option<()>) -> Option<()> {
    match ty {
        Type::Array(x) => op(&mut x.elem),
        Type::Group(x) => op(&mut x.elem),
        Type::Never(_) => Some(()),
        Type::Paren(x) => op(&mut x.elem),
        Type::Path(x) => {
            if let Some(qself) = &mut x.qself {
                op(&mut qself.ty)?;
            }
            for p in x.path.segments.iter_mut() {
                match &mut p.arguments {
                    PathArguments::None => {}
                    PathArguments::AngleBracketed(x) => {
                        x.args.iter_mut().try_for_each(|x| match x {
                            GenericArgument::Type(x) => op(x),
                            _ => Some(()),
                        })?
                    }
                    PathArguments::Parenthesized(x) => {
                        x.inputs.iter_mut().try_for_each(&op)?;
                        if let ReturnType::Type(_, ty) = &mut x.output {
                            op(ty)?;
                        }
                    }
                }
            }
            Some(())
        }
        Type::Ptr(x) => op(&mut x.elem),
        Type::Reference(x) => op(&mut x.elem),
        Type::Slice(x) => op(&mut x.elem),
        Type::Tuple(xs) => xs.elems.iter_mut().try_for_each(op),
        _ => None,
    }
}

// Taken from ref_cast: https://github.com/dtolnay/ref-cast/blob/eadd839cc0db116e4fc9bfee5dd29dde82089eed/derive/src/lib.rs#L91
fn check_repr(input: &DeriveInput) -> syn::Result<()> {
    let mut has_repr = false;
    let mut errors = None;
    let mut push_error = |error| match &mut errors {
        Some(errors) => Error::combine(errors, error),
        None => errors = Some(error),
    };

    for attr in &input.attrs {
        if attr.path.is_ident("repr") {
            if let Err(error) = attr.parse_args_with(|input: ParseStream| {
                while !input.is_empty() {
                    let path = input.call(Path::parse_mod_style)?;
                    if path.is_ident("C") || path.is_ident("transparent") {
                        has_repr = true;
                    } else if path.is_ident("packed") {
                        // ignore
                    } else {
                        let meta_item_span = if input.peek(token::Paren) {
                            let group: TokenTree = input.parse()?;
                            quote!(#path #group)
                        } else if input.peek(Token![=]) {
                            let eq_token: Token![=] = input.parse()?;
                            let value: Expr = input.parse()?;
                            quote!(#path #eq_token #value)
                        } else {
                            quote!(#path)
                        };
                        let msg = if path.is_ident("align") {
                            "aligned repr on struct that implements Coerce is not supported"
                        } else {
                            "unrecognized repr on struct that implements Coerce"
                        };
                        push_error(Error::new_spanned(meta_item_span, msg));
                    }
                    if !input.is_empty() {
                        input.parse::<Token![,]>()?;
                    }
                }
                Ok(())
            }) {
                push_error(error);
            }
        }
    }

    if !has_repr {
        let mut requires_repr = Error::new(
            Span::call_site(),
            "Coerce trait requires #[repr(C)] or #[repr(transparent)]",
        );
        if let Some(errors) = errors {
            requires_repr.combine(errors);
        }
        errors = Some(requires_repr);
    }

    match errors {
        None => Ok(()),
        Some(errors) => Err(errors),
    }
}
