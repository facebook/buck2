/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use allocative::Allocative;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use buck2_core::target::label::TargetLabelMaybeConfigured;

use crate::attrs::attr_type::attr_like::AttrLike;
use crate::attrs::attr_type::configured_dep::ConfiguredExplicitConfiguredDep;
use crate::attrs::attr_type::configured_dep::UnconfiguredExplicitConfiguredDep;
use crate::attrs::attr_type::dep::ExplicitConfiguredDepMaybeConfigured;
use crate::attrs::attr_type::split_transition_dep::ConfiguredSplitTransitionDep;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDep;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDepMaybeConfigured;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::fmt_context::AttrFmtContext;

/// AttrConfig is used to implement things just once to cover both the configured and
/// unconfigured case. For example, a Vec<C::TargetType> where C: AttrConfig, would be
/// a `Vec<TargetLabel>` in the unconfigured case and a `Vec<ConfiguredTargetLabel>` in the
/// configured case.
///
/// For attributes, the difference between the coerced value and the configured value is
/// (1) selects are resolved and (2) configurable things are configured. This trait allows
/// most of the attr representation to be shared between those two states.
///
/// There's really just two implementations of this, one for coerced attrs with
/// unconfigured types and one for configured attrs with the configured types.
pub trait AttrConfig: AttrLike + AttrDisplayWithContext {
    type TargetType: TargetLabelMaybeConfigured + AttrLike;
    type ProvidersType: ProvidersLabelMaybeConfigured + AttrLike;
    type SplitTransitionDepType: SplitTransitionDepMaybeConfigured + AttrLike;
    // Used to encapsulate the type encodings for various attr types.
    type ExtraTypes: AttrConfigExtraTypes + Display + Allocative;

    fn to_json(&self, ctx: &AttrFmtContext) -> anyhow::Result<serde_json::Value>;

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool>;
}

/// Needed to support `ExtraTypes` for within `AttrConfig`.
pub trait AttrConfigExtraTypes {
    fn to_json(&self) -> anyhow::Result<serde_json::Value>;

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool>;
}

/// Attribute literal type encoding for ConfiguredAttrs.
#[derive(Allocative, Debug, Clone, Eq, PartialEq, Hash)]
pub enum ConfiguredAttrExtraTypes {
    ExplicitConfiguredDep(Box<ConfiguredExplicitConfiguredDep>),
}

impl Display for ConfiguredAttrExtraTypes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ExplicitConfiguredDep(e) => Display::fmt(e, f),
        }
    }
}

impl AttrConfigExtraTypes for ConfiguredAttrExtraTypes {
    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        match self {
            Self::ExplicitConfiguredDep(e) => e.to_json(),
        }
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        match self {
            Self::ExplicitConfiguredDep(e) => e.any_matches(filter),
        }
    }
}

impl AttrConfig for ConfiguredAttr {
    type TargetType = ConfiguredTargetLabel;
    type ProvidersType = ConfiguredProvidersLabel;
    type SplitTransitionDepType = ConfiguredSplitTransitionDep;
    type ExtraTypes = ConfiguredAttrExtraTypes;

    fn to_json(&self, ctx: &AttrFmtContext) -> anyhow::Result<serde_json::Value> {
        self.0.to_json(ctx)
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        self.0.any_matches(filter)
    }
}

/// Attribute literal type encoding for CoercedAttrs.
#[derive(Allocative, Debug, Clone, Eq, PartialEq, Hash)]
pub enum CoercedAttrExtraTypes {
    ExplicitConfiguredDep(Box<UnconfiguredExplicitConfiguredDep>),
}

impl AttrConfigExtraTypes for CoercedAttrExtraTypes {
    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        match self {
            Self::ExplicitConfiguredDep(e) => e.to_json(),
        }
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        match self {
            Self::ExplicitConfiguredDep(e) => e.any_matches(filter),
        }
    }
}

impl Display for CoercedAttrExtraTypes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ExplicitConfiguredDep(e) => Display::fmt(e, f),
        }
    }
}

impl AttrConfig for CoercedAttr {
    type TargetType = TargetLabel;
    type ProvidersType = ProvidersLabel;
    type SplitTransitionDepType = SplitTransitionDep;
    type ExtraTypes = CoercedAttrExtraTypes;

    fn to_json(&self, ctx: &AttrFmtContext) -> anyhow::Result<serde_json::Value> {
        CoercedAttr::to_json(self, ctx)
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        CoercedAttr::any_matches(self, filter)
    }
}
