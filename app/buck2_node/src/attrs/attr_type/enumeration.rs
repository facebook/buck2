/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::hash::Hash;

use allocative::Allocative;
use buck2_hash::BuckFixedInsertionSet;
use buck2_hash::BuckMutInsertionSet;
use buck2_util::arc_str::ArcStr;
use pagable::Pagable;
use strong_hash::StrongHash;

#[derive(Debug, Pagable, Eq, PartialEq, Hash, Allocative)]
pub struct EnumAttrType {
    pub variants: BuckFixedInsertionSet<ArcStr>,
}

impl StrongHash for EnumAttrType {
    fn strong_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.variants.len().strong_hash(state);
        for v in self.variants.iter() {
            v.strong_hash(state);
        }
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum EnumAttrError {
    #[error("enum.attr() variant names must all be lowercase, got `{0}`")]
    NotLowercase(String),
    #[error("enum.attr() variant names must all be distinct, got repeated `{0}`")]
    DuplicateVariant(String),
}

impl EnumAttrType {
    pub fn new(variants: Vec<String>) -> buck2_error::Result<Self> {
        let mut result = BuckMutInsertionSet::with_capacity(variants.len());
        for x in variants {
            if x != x.to_lowercase() {
                return Err(EnumAttrError::NotLowercase(x).into());
            }
            if result.contains(x.as_str()) {
                return Err(EnumAttrError::DuplicateVariant(x).into());
            }
            result.insert(ArcStr::from(x));
        }
        Ok(Self {
            variants: result.freeze(),
        })
    }

    pub fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attrs.enum([")?;
        for (i, x) in self.variants.iter().enumerate() {
            if i != 0 {
                write!(f, ",")?;
            }
            write!(f, "{x:?}")?;
        }
        write!(f, "]{arg})")
    }
}
