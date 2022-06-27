/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::one_of::OneOfAttrType;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use itertools::Itertools;
use starlark::values::Value;

use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::attr_type::AttrTypeExt;
use crate::attrs::CoercedAttr;

impl AttrTypeCoerce for OneOfAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let mut errs = Vec::new();
        // Bias towards the start of the list - try and use success/failure from first in preference
        for x in &self.xs {
            match x.coerce_item(configurable, ctx, value) {
                Ok(v) => return Ok(v),
                Err(e) => errs.push(e),
            }
        }
        Err(CoercionError::one_of_many(errs))
    }

    fn starlark_type(&self) -> String {
        format!("[{}]", self.xs.iter().map(|x| x.starlark_type()).join(", "))
    }
}
