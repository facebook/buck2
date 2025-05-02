/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::sync::Mutex;

use buck2_artifact::actions::key::ActionKey;
use buck2_core::deferred::key::DeferredHolderKey;
use dupe::Dupe;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;

use crate::build::detailed_aggregated_metrics::implementation::state::DetailedAggregatedMetricsStateTracker;
use crate::build::detailed_aggregated_metrics::types::ActionExecutionMetrics;
use crate::build::detailed_aggregated_metrics::types::TopLevelTargetSpec;
use crate::deferred::calculation::DeferredHolder;

pub(crate) enum DetailedAggregatedMetricsEvent {
    AnalysisStarted(DeferredHolderKey),
    AnalysisComplete(DeferredHolderKey, DeferredHolder),
    ActionExecuted(ActionExecutionMetrics),
}

struct DetailedAggregatedMetricsEventHandlerInner {
    sender: UnboundedSender<DetailedAggregatedMetricsEvent>,
}

#[derive(Clone, Dupe)]
pub struct DetailedAggregatedMetricsEventHandler(Arc<DetailedAggregatedMetricsEventHandlerInner>);

pub(crate) fn start_detailed_aggregated_metrics_state_tracker()
-> DetailedAggregatedMetricsEventHandler {
    DetailedAggregatedMetricsStateTracker::start()
}

impl DetailedAggregatedMetricsEventHandler {
    pub(crate) fn new() -> (Self, UnboundedReceiver<DetailedAggregatedMetricsEvent>) {
        let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
        (
            Self(Arc::new(DetailedAggregatedMetricsEventHandlerInner {
                sender,
            })),
            receiver,
        )
    }

    pub fn action_executed(&self, ev: ActionExecutionMetrics) {
        self.0
            .sender
            .send(DetailedAggregatedMetricsEvent::ActionExecuted(ev))
            .expect(
                "DetailedAggregagatedMetrics event handler should never exit while sender lives",
            );
    }

    pub fn analysis_started(&self, key: &DeferredHolderKey) {
        self.0
            .sender
            .send(DetailedAggregatedMetricsEvent::AnalysisStarted(key.dupe()))
            .expect(
                "DetailedAggregagatedMetrics event handler should never exit while sender lives",
            );
    }

    pub fn analysis_complete(&self, key: &DeferredHolderKey, result: &DeferredHolder) {
        self.0
            .sender
            .send(DetailedAggregatedMetricsEvent::AnalysisComplete(
                key.dupe(),
                result.dupe(),
            ))
            .expect(
                "DetailedAggregagatedMetrics event handler should never exit while sender lives",
            );
    }
}

pub enum PerBuildEvent {
    ActionExecuted(ActionKey),
    TopLevelTarget(TopLevelTargetSpec),
}

pub(crate) struct DetailedAggregatedMetricsPerBuildEventsHolder {
    events: Mutex<Vec<PerBuildEvent>>,
}

impl DetailedAggregatedMetricsPerBuildEventsHolder {
    pub(crate) fn new() -> Self {
        Self {
            events: Mutex::new(Vec::new()),
        }
    }

    pub(crate) fn action_executed(&self, key: &ActionKey) {
        self.events
            .lock()
            .unwrap()
            .push(PerBuildEvent::ActionExecuted(key.dupe()));
    }

    pub(crate) fn top_level_target(&self, spec: TopLevelTargetSpec) {
        self.events
            .lock()
            .unwrap()
            .push(PerBuildEvent::TopLevelTarget(spec));
    }
}
