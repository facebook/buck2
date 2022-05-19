/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    fmt,
    hash::{Hash, Hasher},
};

use indexmap::IndexSet;
use starlark::values::{string::STRING_TYPE, Value};
use thiserror::Error;

use crate::attrs::{
    attr_type::attr_literal::{AttrLiteral, CoercionError},
    AttrCoercionContext, CoercedAttr,
};

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct EnumAttrType {
    variants: IndexSet<String>,
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for EnumAttrType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.variants.len().hash(state);
        for x in self.variants.iter() {
            x.hash(state);
        }
    }
}

#[derive(Debug, Error)]
enum EnumAttrError {
    #[error("enum.attr() variant names must all be lowercase, got `{0}`")]
    NotLowercase(String),
    #[error("enum.attr() variant names must all be distinct, got repeated `{0}`")]
    DuplicateVariant(String),
}

impl EnumAttrType {
    pub fn new(variants: Vec<String>) -> anyhow::Result<Self> {
        let mut result = IndexSet::with_capacity(variants.len());
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

    pub(crate) fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attr.enum([")?;
        for (i, x) in self.variants.iter().enumerate() {
            if i != 0 {
                write!(f, ",")?;
            }
            write!(f, "{:?}", x)?;
        }
        write!(f, "]{})", arg)
    }

    pub(crate) fn coerce_item(
        &self,
        _ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        match value.unpack_str() {
            Some(s) => {
                // Enum names in Buck can be specified upper or lower case,
                // so we normalise them to lowercase to make rule implementations easier
                let s = s.to_lowercase();
                if self.variants.contains(&s) {
                    Ok(AttrLiteral::String(s))
                } else {
                    Err(
                        CoercionError::invalid_enum(&s, self.variants.iter().cloned().collect())
                            .into(),
                    )
                }
            }
            None => Err(CoercionError::type_error(STRING_TYPE, value).into()),
        }
    }

    pub(crate) fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}
