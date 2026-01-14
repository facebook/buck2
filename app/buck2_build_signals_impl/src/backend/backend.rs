/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_analysis::analysis::calculation::AnalysisWithExtraData;
use buck2_build_api::actions::calculation::ActionWithExtraData;
use buck2_build_signals::env::CriticalPathBackendName;
use buck2_build_signals::env::NodeDuration;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_events::span::SpanId;
use smallvec::SmallVec;

use crate::ActionNodeData;
use crate::AnalysisNodeData;
use crate::BuildInfo;
use crate::NodeDataInner;
use crate::NodeKey;

pub(crate) enum NodeExtraData {
    /// The RegisteredAction that corresponds to this Evaluation (this will only be present for
    /// NodeKey::BuildKey).
    Action(ActionWithExtraData),
    /// This will only be present for NodeKey::AnalysisKey.
    Analysis(AnalysisWithExtraData),
    None,
}

impl From<NodeExtraData> for NodeDataInner {
    fn from(val: NodeExtraData) -> Self {
        match val {
            NodeExtraData::Action(action_with_extra_data) => {
                NodeDataInner::Action(ActionNodeData::from_extra_data(action_with_extra_data))
            }
            NodeExtraData::Analysis(analysis_with_extra_data) => {
                NodeDataInner::Analysis(AnalysisNodeData::from_extra_data(analysis_with_extra_data))
            }
            NodeExtraData::None => NodeDataInner::None,
        }
    }
}

pub(crate) trait BuildListenerBackend {
    fn process_node(
        &mut self,
        key: NodeKey,
        extra_data: NodeExtraData,
        duration: NodeDuration,
        dep_keys: impl IntoIterator<Item = NodeKey>,
        span_ids: SmallVec<[SpanId; 1]>,
    );

    fn process_top_level_target(
        &mut self,
        analysis: ConfiguredTargetLabel,
        artifacts: impl IntoIterator<Item = NodeKey>,
    );

    fn finish(self) -> buck2_error::Result<BuildInfo>;

    fn name() -> CriticalPathBackendName;
}
