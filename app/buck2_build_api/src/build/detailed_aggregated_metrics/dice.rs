/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future::Future;

use buck2_artifact::actions::key::ActionKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_data::ComputeDetailedAggregatedMetricsEnd;
use buck2_data::ComputeDetailedAggregatedMetricsStart;
use buck2_error::internal_error;
use buck2_events::dispatch::span_async_simple;
use dice::DiceComputations;
use dice::DiceDataBuilder;
use dice::UserComputationData;

use crate::build::detailed_aggregated_metrics::events::DetailedAggregatedMetricsEventHandler;
use crate::build::detailed_aggregated_metrics::events::DetailedAggregatedMetricsPerBuildEventsHolder;
use crate::build::detailed_aggregated_metrics::types::ActionExecutionMetrics;
use crate::build::detailed_aggregated_metrics::types::DetailedAggregatedMetrics;
use crate::build::detailed_aggregated_metrics::types::PerBuildEvents;
use crate::build::detailed_aggregated_metrics::types::TopLevelTargetSpec;
use crate::build::graph_properties::GraphPropertiesOptions;
use crate::build::sketch_impl::MergeableGraphSketch;
use crate::deferred::calculation::DeferredHolder;

pub trait HasDetailedAggregatedMetrics {
    fn action_executed(&self, ev: ActionExecutionMetrics) -> buck2_error::Result<()>;
    fn analysis_started(&self, key: &DeferredHolderKey) -> buck2_error::Result<()>;
    fn analysis_complete(
        &self,
        key: &DeferredHolderKey,
        result: &DeferredHolder,
    ) -> buck2_error::Result<()>;
    fn top_level_target(&self, spec: TopLevelTargetSpec) -> buck2_error::Result<()>;
    fn take_per_build_events(&self) -> buck2_error::Result<PerBuildEvents>;
    fn compute_detailed_metrics(
        &self,
        events: PerBuildEvents,
        graph_properties: GraphPropertiesOptions,
    ) -> impl Future<Output = buck2_error::Result<DetailedAggregatedMetrics>> + Send;
    fn compute_action_graph_sketch(
        &self,
        events: &PerBuildEvents,
    ) -> impl Future<Output = buck2_error::Result<Option<MergeableGraphSketch<ActionKey>>>> + Send;
}

impl HasDetailedAggregatedMetrics for DiceComputations<'_> {
    fn top_level_target(&self, spec: TopLevelTargetSpec) -> buck2_error::Result<()> {
        get_per_build_events_holder(self)?.top_level_target(spec);
        Ok(())
    }

    fn action_executed(&self, ev: ActionExecutionMetrics) -> buck2_error::Result<()> {
        get_per_build_events_holder(self)?.action_executed(&ev.key);
        if let Some(v) = get_detailed_aggregated_metrics_event_handler(self)? {
            v.action_executed(ev);
        }
        Ok(())
    }

    fn analysis_started(&self, key: &DeferredHolderKey) -> buck2_error::Result<()> {
        if let Some(v) = get_detailed_aggregated_metrics_event_handler(self)? {
            v.analysis_started(key);
        }
        Ok(())
    }

    fn analysis_complete(
        &self,
        key: &DeferredHolderKey,
        result: &DeferredHolder,
    ) -> buck2_error::Result<()> {
        if let Some(v) = get_detailed_aggregated_metrics_event_handler(self)? {
            v.analysis_complete(key, result);
        }
        Ok(())
    }

    fn take_per_build_events(&self) -> buck2_error::Result<PerBuildEvents> {
        get_per_build_events_holder(self)?.take_events()
    }

    async fn compute_detailed_metrics(
        &self,
        events: PerBuildEvents,
        graph_properties: GraphPropertiesOptions,
    ) -> buck2_error::Result<DetailedAggregatedMetrics> {
        span_async_simple(
            ComputeDetailedAggregatedMetricsStart {},
            async move {
                get_detailed_aggregated_metrics_event_handler(self)?
                    .as_ref()
                    .ok_or_else(|| {
                        internal_error!("should have had a detailed aggreged metrics event holder")
                    })?
                    .compute_metrics(events, graph_properties)
                    .await
            },
            ComputeDetailedAggregatedMetricsEnd {},
        )
        .await
    }

    async fn compute_action_graph_sketch(
        &self,
        events: &PerBuildEvents,
    ) -> buck2_error::Result<Option<MergeableGraphSketch<ActionKey>>> {
        let handler = get_detailed_aggregated_metrics_event_handler(self)?;
        match handler.as_ref() {
            Some(h) => {
                h.compute_action_graph_sketch(events.top_level_targets.clone())
                    .await
            }
            None => Ok(None),
        }
    }
}

/// This is set on the global data (rather than per-transaction data) because it tracks the state across builds.
pub trait SetDetailedAggregatedMetricsEventHandler {
    /// We set an Option<> here (rather than have None be implicit by not calling it) to require that a value is set. We use the None case for
    /// tests just so we don't need to startup the tracking there.
    fn set_detailed_aggregated_metrics_event_handler(
        &mut self,
        sender: Option<DetailedAggregatedMetricsEventHandler>,
    );
}

impl SetDetailedAggregatedMetricsEventHandler for DiceDataBuilder {
    fn set_detailed_aggregated_metrics_event_handler(
        &mut self,
        sender: Option<DetailedAggregatedMetricsEventHandler>,
    ) {
        self.set(sender);
    }
}

fn get_detailed_aggregated_metrics_event_handler<'a>(
    ctx: &'a DiceComputations<'_>,
) -> buck2_error::Result<&'a Option<DetailedAggregatedMetricsEventHandler>> {
    ctx.global_data()
        .get::<Option<DetailedAggregatedMetricsEventHandler>>()
        .map_err(|e| internal_error!("global data invalid: {}", e))
}

fn get_per_build_events_holder<'a>(
    ctx: &'a DiceComputations<'_>,
) -> buck2_error::Result<&'a DetailedAggregatedMetricsPerBuildEventsHolder> {
    ctx.per_transaction_data()
        .data
        .get::<DetailedAggregatedMetricsPerBuildEventsHolder>()
        .map_err(|e| internal_error!("per-transaction data invalid: {}", e))
}

pub trait SetDetailedAggregatedMetricsEventsHolder {
    fn set_detailed_aggregated_metrics_events_holder(&mut self);
}

impl SetDetailedAggregatedMetricsEventsHolder for UserComputationData {
    fn set_detailed_aggregated_metrics_events_holder(&mut self) {
        self.data
            .set(DetailedAggregatedMetricsPerBuildEventsHolder::new())
    }
}
