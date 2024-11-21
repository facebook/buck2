/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use starlark_map::ordered_map::OrderedMap;

#[async_trait]
pub trait TransitionCalculation: Send + Sync + 'static {
    /// Apply transition function to configuration and cache the result.
    async fn apply_transition(
        &self,
        ctx: &mut DiceComputations<'_>,
        attrs: &OrderedMap<&str, Arc<ConfiguredAttr>>,
        conf: &ConfigurationData,
        transition_id: &TransitionId,
    ) -> buck2_error::Result<Arc<TransitionApplied>>;
}

pub static TRANSITION_CALCULATION: LateBinding<&'static dyn TransitionCalculation> =
    LateBinding::new("TRANSITION_CALCULATION");

pub static TRANSITION_ATTRS_PROVIDER: LateBinding<&'static dyn TransitionAttrProvider> =
    LateBinding::new("TRANSITION_ATTRS_PROVIDER");

//TODO transition attributes can be added to Rule. Basic idea is this:
// * in RuleCallable fetch TransitionId from transition value using TransitionValue trait
// * add a function like attrs to TransitionValue,
// * call it from RuleCallable, and store in Rule.
// * in TargetNode we have access to Rule.
#[async_trait]
pub trait TransitionAttrProvider: Send + Sync + 'static {
    /// Fetch attribute names accessed by transition function.
    async fn transition_attrs(
        &self,
        ctx: &mut DiceComputations<'_>,
        transition_id: &TransitionId,
    ) -> buck2_error::Result<Option<Arc<[String]>>>;
}
