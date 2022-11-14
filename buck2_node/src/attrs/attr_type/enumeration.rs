/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::hash::Hash;

use allocative::Allocative;
use buck2_core::collections::ordered_set::OrderedSet;

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub struct EnumAttrType {
    pub variants: OrderedSet<String>,
}

#[derive(Debug, thiserror::Error)]
enum EnumAttrError {
    #[error("enum.attr() variant names must all be lowercase, got `{0}`")]
    NotLowercase(String),
    #[error("enum.attr() variant names must all be distinct, got repeated `{0}`")]
    DuplicateVariant(String),
}

impl EnumAttrType {
    pub fn new(variants: Vec<String>) -> anyhow::Result<Self> {
        let mut result = OrderedSet::with_capacity(variants.len());
        for x in variants {
            if x != x.to_lowercase() {
                return Err(EnumAttrError::NotLowercase(x).into());
            }
            if result.contains(&x) {
                return Err(EnumAttrError::DuplicateVariant(x).into());
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
            write!(f, "{:?}", x)?;
        }
        write!(f, "]{})", arg)
    }
}
