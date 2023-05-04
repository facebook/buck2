/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::visibility::VisibilityAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::attrs::coerce::AttrTypeCoerce;
use crate::nodes::unconfigured::parse_visibility;

impl AttrTypeCoerce for VisibilityAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral> {
        // TODO(nga): unnecessary coercion step.
        let coerced_list_of_strings =
            VisibilityAttrType::pretend_attr_type().coerce_item(configurable, ctx, value)?;
        let visibility = parse_visibility(ctx, &CoercedAttr::Literal(coerced_list_of_strings))?;
        Ok(AttrLiteral::Visibility(visibility))
    }

    fn starlark_type(&self) -> String {
        VisibilityAttrType::pretend_attr_type().starlark_type()
    }
}
