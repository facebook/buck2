/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use anyhow::anyhow;
use starlark::values::{string::STRING_TYPE, Value};

use crate::attrs::{
    attr_type::attr_literal::{AttrLiteral, CoercionError},
    AttrCoercionContext, CoercedAttr,
};

#[derive(Debug, Eq, PartialEq, Hash)]
pub(crate) struct StringAttrType;

impl StringAttrType {
    pub(crate) fn coerce_item(
        &self,
        _ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        match value.unpack_str() {
            Some(s) => Ok(AttrLiteral::String(s.to_owned())),
            None => Err(anyhow!(CoercionError::type_error(STRING_TYPE, value))),
        }
    }

    pub(crate) fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}
