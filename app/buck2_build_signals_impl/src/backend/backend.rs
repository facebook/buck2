/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_build_api::actions::RegisteredAction;
use buck2_build_signals::CriticalPathBackendName;
use buck2_build_signals::NodeDuration;
use buck2_events::span::SpanId;
use smallvec::SmallVec;

use crate::BuildInfo;
use crate::NodeKey;

pub(crate) trait BuildListenerBackend {
    fn process_node(
        &mut self,
        key: NodeKey,
        value: Option<Arc<RegisteredAction>>,
        duration: NodeDuration,
        dep_keys: impl Iterator<Item = NodeKey>,
        span_ids: SmallVec<[SpanId; 1]>,
    );

    fn process_top_level_target(
        &mut self,
        analysis: NodeKey,
        artifacts: impl Iterator<Item = NodeKey>,
    );

    fn finish(self) -> anyhow::Result<BuildInfo>;

    fn name() -> CriticalPathBackendName;
}
