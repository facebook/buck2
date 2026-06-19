/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashSet;
use std::future::Future;

use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_data::ComputeDetailedAggregatedMetricsEnd;
use buck2_data::ComputeDetailedAggregatedMetricsStart;
use buck2_error::internal_error;
use buck2_events::dispatch::span_async_simple;
use dice::DiceComputations;
use dice::DiceDataBuilder;
use dice::UserComputationData;
use dupe::Dupe;
use futures::FutureExt;

use crate::build::BuildProviderType;
use crate::build::detailed_aggregated_metrics::buck2_sketches::ArtifactPathSketches;
use crate::build::detailed_aggregated_metrics::buck2_sketches::compute_artifact_path_sketches_for_target;
use crate::build::detailed_aggregated_metrics::events::DetailedAggregatedMetricsHandle;
use crate::build::detailed_aggregated_metrics::events::DetailedAggregatedMetricsPerBuildEventsHolder;
use crate::build::detailed_aggregated_metrics::types::ActionExecutionMetrics;
use crate::build::detailed_aggregated_metrics::types::ActionGraphSketchResult;
use crate::build::detailed_aggregated_metrics::types::ArtifactPathSketchResult;
use crate::build::detailed_aggregated_metrics::types::DetailedAggregatedMetrics;
use crate::build::detailed_aggregated_metrics::types::PerBuildEvents;
use crate::build::detailed_aggregated_metrics::types::TopLevelTargetSpec;
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
    /// Enable the tracker if `config` requests a consumer of it. Call at command
    /// start; latches on for the daemon's lifetime.
    fn maybe_enable_detailed_aggregated_metrics(
        &self,
        config: &LegacyBuckConfig,
    ) -> buck2_error::Result<()>;
    fn compute_detailed_metrics(
        &self,
        events: PerBuildEvents,
    ) -> impl Future<Output = buck2_error::Result<DetailedAggregatedMetrics>> + Send;
    fn compute_action_graph_sketch(
        &self,
        events: &PerBuildEvents,
    ) -> impl Future<Output = buck2_error::Result<ActionGraphSketchResult>> + Send;
    fn compute_artifact_path_sketch(
        &mut self,
        events: &PerBuildEvents,
        artifact_fs: ArtifactFs,
        providers_to_skip: HashSet<BuildProviderType>,
        sketch_count: bool,
        sketch_size: bool,
    ) -> impl Future<Output = buck2_error::Result<ArtifactPathSketchResult>> + Send;
}

impl HasDetailedAggregatedMetrics for DiceComputations<'_> {
    fn top_level_target(&self, spec: TopLevelTargetSpec) -> buck2_error::Result<()> {
        get_per_build_events_holder(self)?.top_level_target(spec);
        Ok(())
    }

    fn action_executed(&self, ev: ActionExecutionMetrics) -> buck2_error::Result<()> {
        get_per_build_events_holder(self)?.action_executed(&ev.key);
        get_detailed_aggregated_metrics_handle(self)?.action_executed(ev)
    }

    fn analysis_started(&self, key: &DeferredHolderKey) -> buck2_error::Result<()> {
        get_detailed_aggregated_metrics_handle(self)?.analysis_started(key)
    }

    fn analysis_complete(
        &self,
        key: &DeferredHolderKey,
        result: &DeferredHolder,
    ) -> buck2_error::Result<()> {
        get_detailed_aggregated_metrics_handle(self)?.analysis_complete(key, result)
    }

    fn take_per_build_events(&self) -> buck2_error::Result<PerBuildEvents> {
        get_per_build_events_holder(self)?.take_events()
    }

    fn maybe_enable_detailed_aggregated_metrics(
        &self,
        config: &LegacyBuckConfig,
    ) -> buck2_error::Result<()> {
        if detailed_aggregated_metrics_requested(config)? {
            get_detailed_aggregated_metrics_handle(self)?.enable();
        }
        Ok(())
    }

    async fn compute_detailed_metrics(
        &self,
        events: PerBuildEvents,
    ) -> buck2_error::Result<DetailedAggregatedMetrics> {
        span_async_simple(
            ComputeDetailedAggregatedMetricsStart {},
            async move {
                get_detailed_aggregated_metrics_handle(self)?
                    .compute_metrics(events)
                    .await
            },
            ComputeDetailedAggregatedMetricsEnd {},
        )
        .await
    }

    async fn compute_action_graph_sketch(
        &self,
        events: &PerBuildEvents,
    ) -> buck2_error::Result<ActionGraphSketchResult> {
        get_detailed_aggregated_metrics_handle(self)?
            .compute_action_graph_sketch(events.top_level_targets.clone())
            .await
    }

    async fn compute_artifact_path_sketch(
        &mut self,
        events: &PerBuildEvents,
        artifact_fs: ArtifactFs,
        providers_to_skip: HashSet<BuildProviderType>,
        sketch_count: bool,
        sketch_size: bool,
    ) -> buck2_error::Result<ArtifactPathSketchResult> {
        let results = self
            .compute_join(events.top_level_targets.iter(), |ctx, spec| {
                let label = spec.label.clone();
                let outputs = spec.outputs.dupe();
                let artifact_fs = artifact_fs.clone();
                let providers_to_skip = providers_to_skip.clone();
                async move {
                    match compute_artifact_path_sketches_for_target(
                        ctx,
                        &outputs,
                        &artifact_fs,
                        &providers_to_skip,
                        sketch_size,
                        sketch_count,
                    )
                    .await
                    {
                        Ok(sketches) => Ok((label, sketches)),
                        Err(e) => {
                            let _ignored = buck2_core::soft_error!(
                                "artifact_path_sketch_computation_error",
                                e,
                                quiet: true
                            );
                            Ok((label, ArtifactPathSketches::empty()))
                        }
                    }
                }
                .boxed()
            })
            .await;

        let per_target_sketches = results
            .into_iter()
            .collect::<buck2_error::Result<Vec<_>>>()?;
        Ok(ArtifactPathSketchResult {
            per_target_sketches,
        })
    }
}

/// Set on the global data (rather than per-transaction data) because the tracker
/// is a daemon-global handle whose state persists across builds.
pub trait SetDetailedAggregatedMetricsHandle {
    fn set_detailed_aggregated_metrics_handle(&mut self, handle: DetailedAggregatedMetricsHandle);
}

impl SetDetailedAggregatedMetricsHandle for DiceDataBuilder {
    fn set_detailed_aggregated_metrics_handle(&mut self, handle: DetailedAggregatedMetricsHandle) {
        self.set(handle);
    }
}

fn get_detailed_aggregated_metrics_handle<'a>(
    ctx: &'a DiceComputations<'_>,
) -> buck2_error::Result<&'a DetailedAggregatedMetricsHandle> {
    ctx.global_data()
        .get::<DetailedAggregatedMetricsHandle>()
        .map_err(|e| internal_error!("global data invalid: {}", e))
}

/// Whether `config` requests a tracker consumer. (Artifact-path sketches compute
/// from DICE directly and don't read the tracker.)
fn detailed_aggregated_metrics_requested(config: &LegacyBuckConfig) -> buck2_error::Result<bool> {
    for property in ["detailed_aggregated_metrics", "log_action_graph_sketch"] {
        if config
            .parse::<bool>(BuckconfigKeyRef {
                section: "buck2",
                property,
            })?
            .unwrap_or(false)
        {
            return Ok(true);
        }
    }
    Ok(false)
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
