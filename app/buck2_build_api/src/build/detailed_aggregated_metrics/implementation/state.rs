/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_artifact::actions::key::ActionKey;
use buck2_core::deferred::key::DeferredHolderKey;
use dupe::Dupe;

use crate::build::detailed_aggregated_metrics::events::DetailedAggregatedMetricsEvent;
use crate::build::detailed_aggregated_metrics::events::DetailedAggregatedMetricsEventHandler;
use crate::build::detailed_aggregated_metrics::types::ActionExecutionMetrics;
use crate::deferred::calculation::DeferredHolder;

/// Tracks the state required to compute aggregated metrics.
///
/// This tracks all the observed analysis results and action executions. This allows us to traverse
/// the full action graph (including incomplete action graphs when dynamic outputs analysis nodes/inputs
/// fail) and compute the aggregated metrics for all the actions.
///
/// This stores data about the most recently seen analysis/action for each target, regardless of which
/// build it occurred in. We expect the user to track which executions are relevant to the current build,
/// and use that later to compute metrics both over the whole graph and just specific to the current build.
pub struct DetailedAggregatedMetricsStateTracker {
    observed_executions: fxhash::FxHashMap<ActionKey, ActionExecutionMetrics>,
    analysis_nodes: fxhash::FxHashMap<DeferredHolderKey, DeferredHolder>,
}

impl DetailedAggregatedMetricsStateTracker {
    pub(crate) fn start() -> DetailedAggregatedMetricsEventHandler {
        let (event_handler, mut event_receiver) = DetailedAggregatedMetricsEventHandler::new();

        tokio::task::spawn(async move {
            let mut state = Self::new();
            while let Some(v) = event_receiver.recv().await {
                state.event(v);
            }
        });

        event_handler
    }

    fn new() -> Self {
        Self {
            analysis_nodes: fxhash::FxHashMap::default(),
            observed_executions: fxhash::FxHashMap::default(),
        }
    }

    pub(crate) fn event(&mut self, ev: DetailedAggregatedMetricsEvent) {
        match ev {
            DetailedAggregatedMetricsEvent::AnalysisStarted(key) => {
                self.analysis_nodes.remove(&key);
            }
            DetailedAggregatedMetricsEvent::AnalysisComplete(key, data) => {
                self.analysis_nodes.insert(key, data);
            }
            DetailedAggregatedMetricsEvent::ActionExecuted(metrics) => {
                self.observed_executions.insert(metrics.key.dupe(), metrics);
            }
        }
    }
}
