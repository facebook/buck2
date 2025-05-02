/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_artifact::actions::key::ActionKey;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;

use crate::artifact_groups::ArtifactGroup;
use crate::build::BuildProviderType;

#[derive(Clone)]
pub struct ActionExecutionMetrics {
    pub key: ActionKey,
    pub execution_time_ms: u64,
    pub execution_kind: buck2_data::ActionExecutionKind,
    pub output_size_bytes: u64,
}

pub struct TopLevelTargetSpec {
    pub label: Arc<ConfiguredProvidersLabel>,
    pub target: ConfiguredTargetNode,
    pub outputs: Arc<Vec<(ArtifactGroup, BuildProviderType)>>,
}

#[derive(Default)]
pub struct PerBuildEvents {
    pub executed_actions: fxhash::FxHashSet<ActionKey>,
    pub top_level_targets: Vec<TopLevelTargetSpec>,
}
