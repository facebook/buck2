/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::split_transition_dep::SplitTransitionDep;
use buck2_node::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use gazebo::dupe::Dupe;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::coerce::error::CoercionError;
use crate::attrs::coerce::AttrTypeCoerce;

impl AttrTypeCoerce for SplitTransitionDepAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let label = value
            .unpack_str()
            .ok_or_else(|| CoercionError::type_error(STRING_TYPE, value))?;

        let label = ctx.coerce_label(label)?;

        Ok(AttrLiteral::SplitTransitionDep(box SplitTransitionDep {
            label,
            transition: self.transition.dupe(),
            required_providers: self.required_providers.dupe(),
        }))
    }

    fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}
