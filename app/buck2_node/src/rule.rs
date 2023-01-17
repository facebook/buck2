/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::configuration::transition::id::TransitionId;

use crate::attrs::spec::AttributeSpec;
use crate::nodes::unconfigured::RuleKind;
use crate::rule_type::RuleType;

/// Common rule data needed in `TargetNode`.
#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub struct Rule {
    /// The attribute spec. This holds the attribute name -> index mapping and the default values
    /// (for those attributes without explicit values).
    pub attributes: AttributeSpec,
    /// The 'type', used to find the implementation function from the graph
    pub rule_type: RuleType,
    /// The kind of rule, e.g. configuration or otherwise.
    pub rule_kind: RuleKind,
    /// Transition to apply to the target.
    pub cfg: Option<Arc<TransitionId>>,
}
