/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_core::provider::label::ProvidersLabel;
use dupe::Dupe;
use pagable::Pagable;

use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;

/// Describes where a configuration dep appears
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Pagable, Allocative, Dupe)]
pub enum ConfigurationDepKind {
    SelectKey,
    CompatibilityAttribute,
    ConfiguredDepPlatform,
    Transition,
}

/// A configuration dep attribute accepts a target as a value. This is different from
/// a dep in that the values themselves never undergo configuration and appear as bare
/// unconfigured labels even in the configured node. While the values aren't configured,
/// the attribute still is and so selects are still resolved and other values in the
/// attribute could be configured (for example, a `attrs.dict(attrs.dep(), attrs.configuration_dep())`
/// would have the keys configured).
///
/// This is generally used for things that refer to configuration nodes (like platforms or constraints)
/// in attributes like `target_compatible_with` or `exec_compatible_with`.
///
/// They resolve to just the string form of the target and so aren't particularly useful to UDR
/// directly (they are used by the framework).
#[derive(Debug, Eq, PartialEq, Hash, Pagable, Allocative, Clone, Copy, Dupe)]
pub struct ConfigurationDepAttrType(pub ConfigurationDepKind);

impl ConfigurationDepAttrType {
    pub(crate) fn configure(
        _ctx: &dyn AttrConfigurationContext,
        label: &ProvidersLabel,
    ) -> buck2_error::Result<ConfiguredAttr> {
        Ok(ConfiguredAttr::ConfigurationDep(label.dupe()))
    }
}
