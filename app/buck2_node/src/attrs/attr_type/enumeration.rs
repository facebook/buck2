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
use buck2_util::arc_str::ArcStr;
use pagable::Pagable;
use starlark_map::ordered_set::OrderedSet;

#[derive(Debug, Pagable, Eq, PartialEq, Hash, Allocative)]
pub struct EnumAttrType {
    pub variants: OrderedSet<ArcStr>,
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
        let mut result = OrderedSet::with_capacity(variants.len());
        for x in variants {
            if x != x.to_lowercase() {
                return Err(EnumAttrError::NotLowercase(x).into());
            }
            let x = ArcStr::from(x);
            if result.contains(&x) {
                return Err(EnumAttrError::DuplicateVariant(x.as_str().to_owned()).into());
            }
            result.insert(x);
        }
        Ok(Self { variants: result })
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
