/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::actions::calculation::ActionWithExtraData;
use buck2_build_signals::env::CriticalPathBackendName;
use buck2_build_signals::env::NodeDuration;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_events::span::SpanId;
use smallvec::SmallVec;

use crate::BuildInfo;
use crate::NodeKey;

pub(crate) trait BuildListenerBackend {
    fn process_node(
        &mut self,
        key: NodeKey,
        value: Option<ActionWithExtraData>,
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
