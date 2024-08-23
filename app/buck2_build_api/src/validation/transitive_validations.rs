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
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use dupe::Dupe;
use starlark::values::OwnedFrozenRef;
use starlark_map::small_set::SmallSet;

use crate::interpreter::rule_defs::provider::builtin::validation_info::FrozenValidationInfo;

/// Efficiently encoded collection of `ValidationInfo` providers for a given target node
/// and all of its recursive dependencies. Forms an optimized/sparse graph tracking
/// only those providers.
#[derive(Debug, Allocative, Dupe, Clone)]
pub struct TransitiveValidations(pub Arc<TransitiveValidationsData>);

#[derive(Debug, Allocative)]
pub struct TransitiveValidationsData {
    /// `ValidationInfo` provider if the current node contains it
    pub info: Option<OwnedFrozenRef<FrozenValidationInfo>>,
    /// If empty it means that there are no transitive dependencies of current node
    /// which contain `ValidationInfo` providers.
    pub children: SmallSet<ConfiguredTargetLabel>,
}
