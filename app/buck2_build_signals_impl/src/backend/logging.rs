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
use buck2_data::QuickUnstableE2eData;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::span::SpanId;
use serde::Deserialize;
use serde::Serialize;
use smallvec::SmallVec;

use crate::backend::backend::BuildListenerBackend;
use crate::BuildInfo;
use crate::NodeKey;

pub(crate) struct LoggingBackend {
    events: EventDispatcher,
}

impl LoggingBackend {
    pub(crate) fn new(events: EventDispatcher) -> Self {
        Self { events }
    }
}

#[derive(Serialize, Deserialize)]
struct Node {
    key: String,
    deps: Vec<String>,
}

impl BuildListenerBackend for LoggingBackend {
    fn process_node(
        &mut self,
        key: NodeKey,
        _value: Option<ActionWithExtraData>,
        _duration: NodeDuration,
        dep_keys: impl IntoIterator<Item = NodeKey>,
        _span_ids: SmallVec<[SpanId; 1]>,
    ) {
        self.events.instant_event(QuickUnstableE2eData {
            key: "critical_path_logging_node".to_owned(),
            data: serde_json::to_string(&Node {
                key: key.to_string(),
                deps: dep_keys.into_iter().map(|v| v.to_string()).collect(),
            })
            .unwrap(),
        });
    }

    fn process_top_level_target(
        &mut self,
        _analysis: ConfiguredTargetLabel,
        _artifacts: impl IntoIterator<Item = NodeKey>,
    ) {
    }

    fn finish(self) -> buck2_error::Result<BuildInfo> {
        Ok(BuildInfo {
            critical_path: Vec::new(),
            num_nodes: 0,
            num_edges: 0,
            top_level_targets: Default::default(),
        })
    }

    fn name() -> CriticalPathBackendName {
        CriticalPathBackendName::Logging
    }
}
