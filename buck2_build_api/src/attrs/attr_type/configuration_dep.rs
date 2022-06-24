/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::anyhow;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::TargetLabel;
use buck2_node::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use gazebo::prelude::*;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;
use thiserror::Error;

use crate::attrs::analysis::AttrResolutionContext;
use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::configurable::AttrIsConfigurable;
use crate::attrs::AttrCoercionContext;
use crate::attrs::CoercedAttr;
use crate::attrs::ConfiguredAttr;

#[derive(Debug, Error)]
pub enum ResolutionError {
    #[error("attr.configuration_dep() attributes shouldn't have any subtargets, but got `{0}`")]
    UnexpectedSubTarget(ProvidersLabel),
}

pub(crate) trait ConfigurationDepAttrTypeExt {
    fn configure(
        _ctx: &dyn AttrConfigurationContext,
        label: &TargetLabel,
    ) -> anyhow::Result<AttrLiteral<ConfiguredAttr>> {
        Ok(AttrLiteral::ConfigurationDep(label.dupe()))
    }

    fn resolve_single<'v>(
        ctx: &'v dyn AttrResolutionContext,
        label: &TargetLabel,
    ) -> anyhow::Result<Value<'v>> {
        Ok(ctx.heap().alloc(label.to_string()))
    }
}

impl ConfigurationDepAttrTypeExt for ConfigurationDepAttrType {}

impl AttrTypeCoerce for ConfigurationDepAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let label = value
            .unpack_str()
            .ok_or_else(|| anyhow!(CoercionError::type_error(STRING_TYPE, value)))?;

        let label = ctx.coerce_label(label)?;

        let (label, name) = label.into_parts();
        match name {
            ProvidersName::Default => Ok(AttrLiteral::ConfigurationDep(label)),
            _ => Err(ResolutionError::UnexpectedSubTarget(ProvidersLabel::new(label, name)).into()),
        }
    }

    fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}
