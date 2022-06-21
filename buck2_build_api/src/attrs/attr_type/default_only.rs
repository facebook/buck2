/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::values::Value;

use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::AttrCoercionContext;
use crate::attrs::CoercedAttr;

#[derive(Debug, Eq, PartialEq, Hash)]
pub(crate) struct DefaultOnlyAttrType;

impl DefaultOnlyAttrType {
    pub(crate) fn coerce_item(
        &self,
        _ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        Err(CoercionError::default_only(value).into())
    }

    pub(crate) fn starlark_type(&self) -> String {
        "default_only".to_owned()
    }
}
