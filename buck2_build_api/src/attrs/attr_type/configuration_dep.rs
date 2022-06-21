/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::anyhow;
use buck2_core::provider::ProvidersLabel;
use buck2_core::provider::ProvidersName;
use buck2_core::target::TargetLabel;
use gazebo::prelude::*;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;
use thiserror::Error;

use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::AttrCoercionContext;
use crate::attrs::AttrConfigurationContext;
use crate::attrs::AttrResolutionContext;
use crate::attrs::CoercedAttr;
use crate::attrs::ConfiguredAttr;

#[derive(Debug, Error)]
pub enum ResolutionError {
    #[error("attr.configuration_dep() attributes shouldn't have any subtargets, but got `{0}`")]
    UnexpectedSubTarget(ProvidersLabel),
}

/// A configuration dep attribute accepts a target as a value. This is different from
/// a dep in that the values themselves never undergo configuration and appear as bare
/// unconfigured labels even in the configured node. While the values aren't configured,
/// the attribute still is and so selects are still resolved and other values in the
/// attribute could be configured (for example, a `attr.dict(attr.dep(), attr.configuration_dep())`
/// would have the keys configured).
///
/// This is generally used for things that refer to configuration nodes (like platforms or constraints)
/// in attributes like `target_compatible_with` or `exec_compatible_with`.
///
/// They resolve to just the string form of the target and so aren't particularly useful to UDR
/// directly (they are used by the framework).
#[derive(Debug, Eq, PartialEq, Hash)]
pub(crate) struct ConfigurationDepAttrType;

impl ConfigurationDepAttrType {
    pub(crate) fn configure(
        _ctx: &dyn AttrConfigurationContext,
        label: &TargetLabel,
    ) -> anyhow::Result<AttrLiteral<ConfiguredAttr>> {
        Ok(AttrLiteral::ConfigurationDep(label.dupe()))
    }

    pub(crate) fn resolve_single<'v>(
        ctx: &'v dyn AttrResolutionContext,
        label: &TargetLabel,
    ) -> anyhow::Result<Value<'v>> {
        Ok(ctx.heap().alloc(label.to_string()))
    }
}

impl ConfigurationDepAttrType {
    pub(crate) fn coerce_item(
        &self,
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

    pub(crate) fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}
