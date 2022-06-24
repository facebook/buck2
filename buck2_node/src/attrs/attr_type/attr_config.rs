/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use buck2_core::target::TargetLabelMaybeConfigured;

use crate::attrs::attr_type::attr_like::AttrLike;
use crate::attrs::attr_type::dep::ExplicitConfiguredDepMaybeConfigured;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDepMaybeConfigured;

/// AttrConfig is used to implement things just once to cover both the configured and
/// unconfigured case. For example, a Vec<C::TargetType> where C: AttrConfig, would be
/// a Vec<TargetLabel> in the unconfigured case and a Vec<ConfiguredTargetLabel> in the
/// configured case.
///
/// For attributes, the difference between the coerced value and the configured value is
/// (1) selects are resolved and (2) configurable things are configured. This trait allows
/// most of the attr representation to be shared between those two states.
///
/// There's really just two implementations of this, one for coerced attrs with
/// unconfigured types and one for configured attrs with the configured types.
pub trait AttrConfig: AttrLike {
    type TargetType: TargetLabelMaybeConfigured + AttrLike;
    type ProvidersType: ProvidersLabelMaybeConfigured + AttrLike;
    type SplitTransitionDepType: SplitTransitionDepMaybeConfigured + AttrLike;
    type ExplicitConfiguredDepType: ExplicitConfiguredDepMaybeConfigured + AttrLike;

    fn to_json(&self) -> anyhow::Result<serde_json::Value>;

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool>;
}
