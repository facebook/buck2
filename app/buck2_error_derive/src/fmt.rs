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
use std::collections::HashMap as Map;

use proc_macro2::TokenTree;
use quote::format_ident;
use quote::quote_spanned;
use syn::Ident;
use syn::Index;
use syn::LitStr;
use syn::Member;
use syn::Result;
use syn::Token;
use syn::ext::IdentExt;
use syn::parse::ParseStream;
use syn::parse::Parser;

use crate::ast::Field;
use crate::attr::Display;
use crate::attr::Trait;

impl Display<'_> {
    // Transform `"error {var}"` to `"error {}", var`.
    pub fn expand_shorthand(&mut self, fields: &[Field]) {
        let raw_args = self.args.clone();
        let mut named_args = explicit_named_args.parse2(raw_args).unwrap();
        let mut member_index = Map::new();
        for (i, field) in fields.iter().enumerate() {
            member_index.insert(&field.member, i);
        }

        let span = self.fmt.span();
        let fmt = self.fmt.value();
        let mut read = fmt.as_str();
        let mut out = String::new();
        let mut args = self.args.clone();
        let mut implied_bounds = Set::new();

        let mut has_trailing_comma = false;
        if let Some(TokenTree::Punct(punct)) = args.clone().into_iter().last() {
            if punct.as_char() == ',' {
                has_trailing_comma = true;
            }
        }

        while let Some(brace) = read.find('{') {
            out += &read[..brace + 1];
            read = &read[brace + 1..];
            if read.starts_with('{') {
                out.push('{');
                read = &read[1..];
                continue;
            }
            let next = match read.chars().next() {
                Some(next) => next,
                None => return,
            };
            let member = match next {
                '0'..='9' => {
                    let int = take_int(&mut read);
                    let member = match int.parse::<u32>() {
                        Ok(index) => Member::Unnamed(Index { index, span }),
                        Err(_) => return,
                    };
                    if !member_index.contains_key(&member) {
                        out += &int;
                        continue;
                    }
                    member
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut ident = take_ident(&mut read);
                    ident.set_span(span);
                    Member::Named(ident)
                }
                _ => continue,
            };
            if let Some(&field) = member_index.get(&member) {
                let end_spec = match read.find('}') {
                    Some(end_spec) => end_spec,
                    None => return,
                };
                let bound = match read[..end_spec].chars().next_back() {
                    Some('?') => Trait::Debug,
                    Some('o') => Trait::Octal,
                    Some('x') => Trait::LowerHex,
                    Some('X') => Trait::UpperHex,
                    Some('p') => Trait::Pointer,
                    Some('b') => Trait::Binary,
                    Some('e') => Trait::LowerExp,
                    Some('E') => Trait::UpperExp,
                    Some(_) | None => Trait::Display,
                };
                implied_bounds.insert((field, bound));
            }
            let local = match &member {
                Member::Unnamed(index) => format_ident!("_{}", index),
                Member::Named(ident) => ident.clone(),
            };
            let mut formatvar = local.clone();
            if formatvar.to_string().starts_with("r#") {
                formatvar = format_ident!("r_{}", formatvar);
            }
            if formatvar.to_string().starts_with('_') {
                // Work around leading underscore being rejected by 1.40 and
                // older compilers. https://github.com/rust-lang/rust/pull/66847
                formatvar = format_ident!("field_{}", formatvar);
            }
            out += &formatvar.to_string();
            if !named_args.insert(formatvar.clone()) {
                // Already specified in the format argument list.
                continue;
            }
            if !has_trailing_comma {
                args.extend(quote_spanned!(span=> ,));
            }
            args.extend(quote_spanned!(span=> #formatvar = #local));
            has_trailing_comma = false;
        }

        out += read;
        self.fmt = LitStr::new(&out, self.fmt.span());
        self.args = args;
        self.implied_bounds = implied_bounds;
    }
}

fn explicit_named_args(input: ParseStream) -> Result<Set<Ident>> {
    let mut named_args = Set::new();

    while !input.is_empty() {
        if input.peek(Token![,]) && input.peek2(Ident::peek_any) && input.peek3(Token![=]) {
            input.parse::<Token![,]>()?;
            let ident = input.call(Ident::parse_any)?;
            input.parse::<Token![=]>()?;
            named_args.insert(ident);
        } else {
            input.parse::<TokenTree>()?;
        }
    }

    Ok(named_args)
}

fn take_int(read: &mut &str) -> String {
    let mut int = String::new();
    for (i, ch) in read.char_indices() {
        match ch {
            '0'..='9' => int.push(ch),
            _ => {
                *read = &read[i..];
                break;
            }
        }
    }
    int
}

fn take_ident(read: &mut &str) -> Ident {
    let mut ident = String::new();
    let raw = read.starts_with("r#");
    if raw {
        ident.push_str("r#");
        *read = &read[2..];
    }
    for (i, ch) in read.char_indices() {
        match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => ident.push(ch),
            _ => {
                *read = &read[i..];
                break;
            }
        }
    }
    Ident::parse_any.parse_str(&ident).unwrap()
}
