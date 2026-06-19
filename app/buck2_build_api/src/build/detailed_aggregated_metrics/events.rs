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
use std::sync::Mutex;
use std::sync::OnceLock;

use buck2_artifact::actions::key::ActionKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_error::internal_error;
use dupe::Dupe;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;

use crate::build::detailed_aggregated_metrics::implementation::state::DetailedAggregatedMetricsStateTracker;
use crate::build::detailed_aggregated_metrics::types::ActionExecutionMetrics;
use crate::build::detailed_aggregated_metrics::types::ActionGraphSketchResult;
use crate::build::detailed_aggregated_metrics::types::DetailedAggregatedMetrics;
use crate::build::detailed_aggregated_metrics::types::PerBuildEvents;
use crate::build::detailed_aggregated_metrics::types::TopLevelTargetSpec;
use crate::deferred::calculation::DeferredHolder;

pub(crate) enum DetailedAggregatedMetricsEvent {
    AnalysisStarted(DeferredHolderKey),
    AnalysisComplete(DeferredHolderKey, DeferredHolder),
    ComputeMetrics(
        PerBuildEvents,
        tokio::sync::oneshot::Sender<buck2_error::Result<DetailedAggregatedMetrics>>,
    ),
    ComputeActionGraphSketch(
        Vec<TopLevelTargetSpec>,
        tokio::sync::oneshot::Sender<buck2_error::Result<ActionGraphSketchResult>>,
    ),
    ActionExecuted(ActionExecutionMetrics),
}

struct DetailedAggregatedMetricsEventHandlerInner {
    sender: UnboundedSender<DetailedAggregatedMetricsEvent>,
}

/// Handle to the running tracker task: a thin wrapper over the channel used to
/// send it events.
#[derive(Clone, Dupe)]
pub(crate) struct DetailedAggregatedMetricsEventHandler(
    Arc<DetailedAggregatedMetricsEventHandlerInner>,
);

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

    fn send(&self, event: DetailedAggregatedMetricsEvent) -> buck2_error::Result<()> {
        self.0.sender.send(event).map_err(|_| {
            internal_error!("DetailedAggregatedMetrics event handler exited while sender lives")
        })?;
        Ok(())
    }

    async fn compute_metrics(
        &self,
        events: PerBuildEvents,
    ) -> buck2_error::Result<DetailedAggregatedMetrics> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.send(DetailedAggregatedMetricsEvent::ComputeMetrics(events, tx))?;
        rx.await?
    }

    async fn compute_action_graph_sketch(
        &self,
        top_level_targets: Vec<TopLevelTargetSpec>,
    ) -> buck2_error::Result<ActionGraphSketchResult> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.send(DetailedAggregatedMetricsEvent::ComputeActionGraphSketch(
            top_level_targets,
            tx,
        ))?;
        rx.await?
    }
}

/// Daemon-global handle stored in DICE global data (so must be `Send + Sync`); the
/// single entry point for recording analyses/actions and computing metrics.
///
/// The tracker task lives in a `OnceLock` so it can be started after the handle
/// is constructed. Today it is always started at construction via [`Self::start`],
/// so collection is on for the daemon's whole lifetime; the `OnceLock` leaves room
/// to start it on demand later. Tests that don't collect metrics use
/// [`Self::disabled`], which leaves the lock empty.
#[derive(Default)]
pub struct DetailedAggregatedMetricsHandle {
    tracker: OnceLock<DetailedAggregatedMetricsEventHandler>,
}

impl DetailedAggregatedMetricsHandle {
    /// Create a handle with a running tracker task.
    pub fn start() -> Self {
        let this = Self::default();
        this.enable();
        this
    }

    /// Create a handle with no tracker, for tests that don't collect metrics.
    pub fn disabled() -> Self {
        Self::default()
    }

    /// Start the tracker task if it isn't already running.
    fn enable(&self) {
        let _ = self
            .tracker
            .get_or_init(DetailedAggregatedMetricsStateTracker::start);
    }

    pub fn action_executed(&self, ev: ActionExecutionMetrics) -> buck2_error::Result<()> {
        match self.tracker.get() {
            Some(tracker) => tracker.send(DetailedAggregatedMetricsEvent::ActionExecuted(ev)),
            None => Ok(()),
        }
    }

    pub fn analysis_started(&self, key: &DeferredHolderKey) -> buck2_error::Result<()> {
        match self.tracker.get() {
            Some(tracker) => {
                tracker.send(DetailedAggregatedMetricsEvent::AnalysisStarted(key.dupe()))
            }
            None => Ok(()),
        }
    }

    pub fn analysis_complete(
        &self,
        key: &DeferredHolderKey,
        result: &DeferredHolder,
    ) -> buck2_error::Result<()> {
        match self.tracker.get() {
            Some(tracker) => tracker.send(DetailedAggregatedMetricsEvent::AnalysisComplete(
                key.dupe(),
                result.dupe(),
            )),
            None => Ok(()),
        }
    }

    pub async fn compute_metrics(
        &self,
        events: PerBuildEvents,
    ) -> buck2_error::Result<DetailedAggregatedMetrics> {
        match self.tracker.get() {
            Some(tracker) => tracker.compute_metrics(events).await,
            None => Err(internal_error!(
                "should have had a detailed aggregated metrics event holder"
            )),
        }
    }

    pub async fn compute_action_graph_sketch(
        &self,
        top_level_targets: Vec<TopLevelTargetSpec>,
    ) -> buck2_error::Result<ActionGraphSketchResult> {
        match self.tracker.get() {
            Some(tracker) => tracker.compute_action_graph_sketch(top_level_targets).await,
            None => Ok(ActionGraphSketchResult {
                per_target_sketches: Vec::new(),
            }),
        }
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

    pub(crate) fn take_events(&self) -> buck2_error::Result<PerBuildEvents> {
        let mut events = PerBuildEvents::default();
        let messages = { std::mem::take(&mut *self.events.lock().unwrap()) };

        for ev in messages {
            match ev {
                PerBuildEvent::ActionExecuted(key) => {
                    events.executed_actions.insert(key);
                }
                PerBuildEvent::TopLevelTarget(spec) => {
                    events.top_level_targets.push(spec);
                }
            }
        }

        Ok(events)
    }
}
