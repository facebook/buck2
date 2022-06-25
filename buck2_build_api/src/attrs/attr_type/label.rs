/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::provider::label::ProvidersLabel;
use buck2_node::attrs::attr_type::label::LabelAttrType;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::AttrCoercionContext;
use crate::attrs::AttrLiteral;

impl AttrTypeCoerce for LabelAttrType {
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

        Ok(AttrLiteral::Label(box label))
    }

    fn starlark_type(&self) -> String {
        "label".to_owned()
    }
}

pub(crate) trait LabelAttrTypeExt {
    fn configure(
        ctx: &dyn AttrConfigurationContext,
        label: &ProvidersLabel,
    ) -> anyhow::Result<AttrLiteral<ConfiguredAttr>>;
}

impl LabelAttrTypeExt for LabelAttrType {
    fn configure(
        ctx: &dyn AttrConfigurationContext,
        label: &ProvidersLabel,
    ) -> anyhow::Result<AttrLiteral<ConfiguredAttr>> {
        Ok(AttrLiteral::Label(box ctx.configure_target(label)))
    }
}
