/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::option::OptionAttrType;
use starlark::values::Value;

use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::attr_type::AttrTypeExt;
use crate::attrs::configurable::AttrIsConfigurable;
use crate::attrs::AttrCoercionContext;
use crate::attrs::CoercedAttr;

impl AttrTypeCoerce for OptionAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        if value.is_none() {
            Ok(AttrLiteral::None)
        } else {
            Ok(self.inner.coerce_item(configurable, ctx, value)?)
        }
    }

    fn starlark_type(&self) -> String {
        format!("[None, {}]", self.inner.starlark_type())
    }
}
