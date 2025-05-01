/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// This code is adapted from https://github.com/dtolnay/thiserror licensed under Apache-2.0 or MIT.

use std::collections::BTreeMap as Map;
use std::collections::BTreeSet as Set;
use std::collections::btree_map::Entry;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::GenericArgument;
use syn::Generics;
use syn::Ident;
use syn::PathArguments;
use syn::Token;
use syn::Type;
use syn::WhereClause;
use syn::parse_quote;
use syn::punctuated::Punctuated;

pub struct ParamsInScope<'a> {
    names: Set<&'a Ident>,
}

impl<'a> ParamsInScope<'a> {
    pub fn new(generics: &'a Generics) -> Self {
        ParamsInScope {
            names: generics.type_params().map(|param| &param.ident).collect(),
        }
    }

    pub fn intersects(&self, ty: &Type) -> bool {
        let mut found = false;
        crawl(self, ty, &mut found);
        found
    }
}

fn crawl(in_scope: &ParamsInScope, ty: &Type, found: &mut bool) {
    if let Type::Path(ty) = ty {
        if ty.qself.is_none() {
            if let Some(ident) = ty.path.get_ident() {
                if in_scope.names.contains(ident) {
                    *found = true;
                }
            }
        }
        for segment in &ty.path.segments {
            if let PathArguments::AngleBracketed(arguments) = &segment.arguments {
                for arg in &arguments.args {
                    if let GenericArgument::Type(ty) = arg {
                        crawl(in_scope, ty, found);
                    }
                }
            }
        }
    }
}

pub struct InferredBounds {
    bounds: Map<String, (Set<String>, Punctuated<TokenStream, Token![+]>)>,
    order: Vec<TokenStream>,
}

impl InferredBounds {
    pub fn new() -> Self {
        InferredBounds {
            bounds: Map::new(),
            order: Vec::new(),
        }
    }

    #[allow(clippy::type_repetition_in_bounds, clippy::trait_duplication_in_bounds)] // clippy bug: https://github.com/rust-lang/rust-clippy/issues/8771
    pub fn insert(&mut self, ty: impl ToTokens, bound: impl ToTokens) {
        let ty = ty.to_token_stream();
        let bound = bound.to_token_stream();
        let entry = self.bounds.entry(ty.to_string());
        if let Entry::Vacant(_) = entry {
            self.order.push(ty);
        }
        let (set, tokens) = entry.or_default();
        if set.insert(bound.to_string()) {
            tokens.push(bound);
        }
    }

    pub fn augment_where_clause(&self, generics: &Generics) -> WhereClause {
        let mut generics = generics.clone();
        let where_clause = generics.make_where_clause();
        for ty in &self.order {
            let (_set, bounds) = &self.bounds[&ty.to_string()];
            where_clause.predicates.push(parse_quote!(#ty: #bounds));
        }
        generics.where_clause.unwrap()
    }
}
