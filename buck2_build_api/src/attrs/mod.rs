/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module provides support for implementing and consuming attr
//! types.
//!
//! Support for implementing new attribute types is in the `val` module.
//!
//! Attribute values are present in 4 different states:
//!   1. initial starlark value - the value passed to the rule function
//!   2. coerced value - the value captured after processing the build file. At this point, the type has
//!      been checked (i.e. for an attr.list(attr.string()), we've confirmed that (1) was an iterable of strings).
//!      This is done when invoking a rule function, so it has no access to configuration or information from
//!      other build files.
//!   3. configured value - this is roughly (2) with a specific configuration attached to all configurable
//!      values. For example a dep or a source (where the source is an output) are configurable (they both are basically
//!      targets).
//!   4. resolved value - this is the "resolved" attribute once again as a starlark value and as provided to a rule
//!      implementation function.
//!
//! Attribute coercion happens immediately on declaring a build target (via
//! invoking a rule function). It will validate the types of the
//! attribute values and do some simple conversions (ex. parse strings to
//! target labels).
//!
//! Attribute configuration happens when the configuration for a target
//! becomes available. There are two primary operations that happen
//! here: select resolution and target configuration.
//!
//! Attribute resolution happens just before invoking the rule
//! implementation. At this point, the context has access to all the
//! providers that are needed (based on inspection of the configured value)
//! and anything that requires a provider can be resolved to its final
//! value.
//!
//! Generally an attribute type is specified as its "resolved" type that the
//! rule implementation requests. For example, a rule that wants a list of files
//! for its sources would specify `attr.list(attr.file())` and would
//! receive a list of files in the implementation. The intermediate form of that
//! may be strings or targets or some other thing (e.g. a lazy glob, maybe).

use anyhow::anyhow;
use attr_type::bool;
use attr_type::split_transition_dep::ConfiguredSplitTransitionDep;
use attr_type::split_transition_dep::SplitTransitionDep;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use buck2_node::attrs::attr_type::attr_config::AttrConfig;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::configured_dep::ConfiguredExplicitConfiguredDep;
use buck2_node::attrs::attr_type::configured_dep::UnconfiguredExplicitConfiguredDep;
use buck2_node::attrs::coerced_path::CoercedPath;

use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::configured_attr::ConfiguredAttr;

pub(crate) mod analysis;
pub mod attr_type;
pub mod coerced_attr;
pub mod configurable;
pub mod configured_attr;

#[cfg(test)]
pub(crate) mod testing;
#[cfg(test)]
mod tests;

/// A simple map that maintains insertion order.
pub type OrderedMap<K, V> = small_map::map::SmallMap<K, V>;
pub type OrderedMapEntry<'a, K, V> = small_map::map::Entry<'a, K, V>;
pub type OrderedMapOccupiedEntry<'a, K, V> = small_map::map::OccupiedEntry<'a, K, V>;
pub type OrderedMapVacantEntry<'a, K, V> = small_map::map::VacantEntry<'a, K, V>;

/// The context for attribute coercion. Mostly just contains information about
/// the current package (to support things like parsing targets from strings).
pub trait AttrCoercionContext {
    fn coerce_target(&self, value: &str) -> anyhow::Result<TargetLabel> {
        let label = self.coerce_label(value)?;
        if let ProvidersName::Named(_) = label.name() {
            return Err(anyhow!(CoercionError::unexpected_providers_name(value)));
        }
        Ok(label.into_parts().0)
    }

    /// Attempt to convert a string into a label
    fn coerce_label(&self, value: &str) -> anyhow::Result<ProvidersLabel>;

    /// Attempt to convert a string into a BuckPath
    fn coerce_path(&self, value: &str, allow_directory: bool) -> anyhow::Result<CoercedPath>;
}

impl AttrConfig for CoercedAttr {
    type TargetType = TargetLabel;
    type ProvidersType = ProvidersLabel;
    type SplitTransitionDepType = SplitTransitionDep;
    type ExplicitConfiguredDepType = UnconfiguredExplicitConfiguredDep;

    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        CoercedAttr::to_json(self)
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        CoercedAttr::any_matches(self, filter)
    }
}

impl AttrConfig for ConfiguredAttr {
    type TargetType = ConfiguredTargetLabel;
    type ProvidersType = ConfiguredProvidersLabel;
    type SplitTransitionDepType = ConfiguredSplitTransitionDep;
    type ExplicitConfiguredDepType = ConfiguredExplicitConfiguredDep;

    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        self.0.to_json()
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        self.0.any_matches(filter)
    }
}
