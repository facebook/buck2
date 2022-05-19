/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::provider::ProvidersLabel;
use gazebo::dupe::*;
use starlark::values::{string::STRING_TYPE, Value};

use crate::attrs::{
    attr_type::attr_literal::CoercionError, coerced_attr::CoercedAttr, AttrCoercionContext,
    AttrConfigurationContext, AttrLiteral, ConfiguredAttr,
};

#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe)]
pub(crate) struct LabelAttrType;

impl LabelAttrType {
    pub(crate) fn coerce_item(
        &self,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let label = value
            .unpack_str()
            .ok_or_else(|| CoercionError::type_error(STRING_TYPE, value))?;

        let label = ctx.coerce_label(label)?;

        Ok(AttrLiteral::Label(box label))
    }

    pub(crate) fn starlark_type(&self) -> String {
        "label".to_owned()
    }

    pub(crate) fn configure(
        ctx: &dyn AttrConfigurationContext,
        label: &ProvidersLabel,
    ) -> anyhow::Result<AttrLiteral<ConfiguredAttr>> {
        Ok(AttrLiteral::Label(box ctx.configure_target(label)))
    }
}
