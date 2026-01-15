/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::plugins::PluginKind;
#[allow(unused_imports)]
use buck2_util::hash::BuckHasher;
use pagable::Pagable;
use static_interner::interner;

use crate::attrs::spec::AttributeSpec;
use crate::nodes::unconfigured::RuleKind;
use crate::rule_type::RuleType;

#[derive(Debug, Eq, PartialEq, Hash, Pagable, Allocative, Clone, dupe::Dupe)]
pub enum RuleIncomingTransition {
    None,
    Fixed(Arc<TransitionId>),
    /// This rule has an `incoming_transition` attribute
    FromAttribute,
}

/// Common rule data needed in `TargetNode`.
#[derive(Debug, Eq, PartialEq, Hash, Pagable, Allocative)]
pub struct Rule {
    /// The attribute spec. This holds the attribute name -> index mapping and the default values
    /// (for those attributes without explicit values).
    pub attributes: AttributeSpec,
    /// The 'type', used to find the implementation function from the graph
    pub rule_type: RuleType,
    /// The kind of rule, e.g. configuration or otherwise.
    pub rule_kind: RuleKind,
    /// Transition to apply to the target.
    pub cfg: RuleIncomingTransition,
    /// The plugin kinds that are used by the target
    pub uses_plugins: Vec<PluginKind>,
}

interner!(INTERNER, BuckHasher, Rule);
