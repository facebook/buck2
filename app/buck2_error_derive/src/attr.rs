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

use proc_macro2::Delimiter;
use proc_macro2::Group;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use quote::format_ident;
use quote::quote;
use quote::ToTokens;
use syn::braced;
use syn::bracketed;
use syn::parenthesized;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::parse::Parser;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token;
use syn::Attribute;
use syn::Error;
use syn::Ident;
use syn::Index;
use syn::LitInt;
use syn::LitStr;
use syn::Result;
use syn::Token;

/// Did the user provide an explicit value for the option, or a function from which to compute it
#[derive(Clone)]
pub enum OptionStyle {
    Explicit(syn::Ident),
    ByExpr(syn::Expr),
}

impl OptionStyle {
    pub fn span(&self) -> Span {
        match self {
            Self::Explicit(ident) => ident.span(),
            Self::ByExpr(expr) => expr.span(),
        }
    }
}

impl Parse for OptionStyle {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // syn does not have `is_empty2`
        let fork = input.fork();
        if fork.parse::<syn::Ident>().is_ok() && (fork.peek(Token![,]) || fork.is_empty()) {
            let ident = input.parse::<syn::Ident>()?;
            Ok(Self::Explicit(ident.clone()))
        } else {
            Ok(Self::ByExpr(input.parse()?))
        }
    }
}

enum MacroOption {
    Tag(OptionStyle),
}

impl Parse for MacroOption {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        if name == "input" {
            let ident = syn::Ident::new("Input", name.span());
            Ok(MacroOption::Tag(OptionStyle::Explicit(ident)))
        } else if name == "tier0" {
            let ident = syn::Ident::new("Tier0", name.span());
            Ok(MacroOption::Tag(OptionStyle::Explicit(ident)))
        } else if name == "environment" {
            let ident = syn::Ident::new("Environment", name.span());
            Ok(MacroOption::Tag(OptionStyle::Explicit(ident)))
        } else if name == "tag" {
            let _eq: Token![=] = input.parse()?;
            Ok(MacroOption::Tag(input.parse()?))
        } else {
            Err(syn::Error::new_spanned(name, "expected option"))
        }
    }
}

pub struct Attrs<'a> {
    pub display: Option<Display<'a>>,
    pub source: Option<&'a Attribute>,
    pub transparent: Option<Transparent<'a>>,
    pub tags: Vec<OptionStyle>,
}

#[derive(Clone)]
pub struct Display<'a> {
    pub original: &'a Attribute,
    pub fmt: LitStr,
    pub args: TokenStream,
    pub implied_bounds: Set<(usize, Trait)>,
}

#[derive(Copy, Clone)]
pub struct Transparent<'a> {
    pub original: &'a Attribute,
    pub span: Span,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum Trait {
    Debug,
    Display,
    Octal,
    LowerHex,
    UpperHex,
    Pointer,
    Binary,
    LowerExp,
    UpperExp,
}

pub fn get(input: &[Attribute]) -> Result<Attrs> {
    let mut attrs = Attrs {
        display: None,
        source: None,
        transparent: None,
        tags: Vec::new(),
    };

    for attr in input {
        if attr.path().is_ident("error") {
            parse_error_attribute(&mut attrs, attr)?;
        } else if attr.path().is_ident("source") {
            attr.meta.require_path_only()?;
            if attrs.source.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[source] attribute"));
            }
            attrs.source = Some(attr);
        } else if attr.path().is_ident("buck2") {
            let meta = attr.meta.require_list()?;
            let parsed = Punctuated::<MacroOption, Token![,]>::parse_terminated
                .parse2(meta.tokens.clone())?;
            for option in parsed {
                match option {
                    MacroOption::Tag(style) => {
                        attrs.tags.push(style);
                    }
                }
            }
        }
    }

    Ok(attrs)
}

fn parse_error_attribute<'a>(attrs: &mut Attrs<'a>, attr: &'a Attribute) -> Result<()> {
    syn::custom_keyword!(transparent);

    attr.parse_args_with(|input: ParseStream| {
        if let Some(kw) = input.parse::<Option<transparent>>()? {
            if attrs.transparent.is_some() {
                return Err(Error::new_spanned(
                    attr,
                    "duplicate #[error(transparent)] attribute",
                ));
            }
            attrs.transparent = Some(Transparent {
                original: attr,
                span: kw.span,
            });
            return Ok(());
        }

        let display = Display {
            original: attr,
            fmt: input.parse()?,
            args: parse_token_expr(input, false)?,
            implied_bounds: Set::new(),
        };
        if attrs.display.is_some() {
            return Err(Error::new_spanned(
                attr,
                "only one #[error(...)] attribute is allowed",
            ));
        }
        attrs.display = Some(display);
        Ok(())
    })
}

fn parse_token_expr(input: ParseStream, mut begin_expr: bool) -> Result<TokenStream> {
    let mut tokens = Vec::new();
    while !input.is_empty() {
        if begin_expr && input.peek(Token![.]) {
            if input.peek2(Ident) {
                input.parse::<Token![.]>()?;
                begin_expr = false;
                continue;
            }
            if input.peek2(LitInt) {
                input.parse::<Token![.]>()?;
                let int: Index = input.parse()?;
                let ident = format_ident!("_{}", int.index, span = int.span);
                tokens.push(TokenTree::Ident(ident));
                begin_expr = false;
                continue;
            }
        }

        begin_expr = input.peek(Token![break])
            || input.peek(Token![continue])
            || input.peek(Token![if])
            || input.peek(Token![in])
            || input.peek(Token![match])
            || input.peek(Token![mut])
            || input.peek(Token![return])
            || input.peek(Token![while])
            || input.peek(Token![+])
            || input.peek(Token![&])
            || input.peek(Token![!])
            || input.peek(Token![^])
            || input.peek(Token![,])
            || input.peek(Token![/])
            || input.peek(Token![=])
            || input.peek(Token![>])
            || input.peek(Token![<])
            || input.peek(Token![|])
            || input.peek(Token![%])
            || input.peek(Token![;])
            || input.peek(Token![*])
            || input.peek(Token![-]);

        let token: TokenTree = if input.peek(token::Paren) {
            let content;
            let delimiter = parenthesized!(content in input);
            let nested = parse_token_expr(&content, true)?;
            let mut group = Group::new(Delimiter::Parenthesis, nested);
            group.set_span(delimiter.span.join());
            TokenTree::Group(group)
        } else if input.peek(token::Brace) {
            let content;
            let delimiter = braced!(content in input);
            let nested = parse_token_expr(&content, true)?;
            let mut group = Group::new(Delimiter::Brace, nested);
            group.set_span(delimiter.span.join());
            TokenTree::Group(group)
        } else if input.peek(token::Bracket) {
            let content;
            let delimiter = bracketed!(content in input);
            let nested = parse_token_expr(&content, true)?;
            let mut group = Group::new(Delimiter::Bracket, nested);
            group.set_span(delimiter.span.join());
            TokenTree::Group(group)
        } else {
            input.parse()?
        };
        tokens.push(token);
    }
    Ok(TokenStream::from_iter(tokens))
}

impl ToTokens for Display<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let fmt = &self.fmt;
        let args = &self.args;
        tokens.extend(quote! {
            std::write!(__formatter, #fmt #args)
        });
    }
}

impl ToTokens for Trait {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let trait_name = format_ident!("{}", format!("{:?}", self));
        tokens.extend(quote!(std::fmt::#trait_name));
    }
}
