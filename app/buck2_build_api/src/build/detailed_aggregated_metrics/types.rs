/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cmp::max;
use std::sync::Arc;

use buck2_artifact::actions::key::ActionKey;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_data::ActionExecutionKind;
use buck2_data::ToProtoMessage;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dupe::Dupe;

use crate::artifact_groups::ArtifactGroup;
use crate::build::BuildProviderType;
use crate::build::sketch_impl::MergeableGraphSketch;

#[derive(Clone)]
pub struct ActionExecutionMetrics {
    pub key: ActionKey,
    pub execution_time_ms: u64,
    pub execution_kind: buck2_data::ActionExecutionKind,
    pub output_size_bytes: u64,
    pub memory_peak: Option<u64>,
}

pub struct AnalysisMetrics {
    pub actions: usize,
    pub retained_memory: usize,
}

#[derive(Clone)]
pub struct TopLevelTargetSpec {
    pub label: ConfiguredProvidersLabel,
    pub target: ConfiguredTargetNode,
    pub outputs: Arc<Vec<(ArtifactGroup, BuildProviderType)>>,
}

#[derive(Default)]
pub struct PerBuildEvents {
    pub executed_actions: fxhash::FxHashSet<ActionKey>,
    pub top_level_targets: Vec<TopLevelTargetSpec>,
}

pub struct DetailedAggregatedMetrics {
    pub top_level_target_metrics: Vec<TopLevelTargetAggregatedData>,
    pub all_targets_build_metrics: AllTargetsAggregatedData,
    pub action_graph_sketch: Option<MergeableGraphSketch<ActionKey>>,
}

impl ToProtoMessage for DetailedAggregatedMetrics {
    type Message = buck2_data::DetailedAggregatedMetrics;

    fn as_proto(&self) -> Self::Message {
        buck2_data::DetailedAggregatedMetrics {
            top_level_target_metrics: self
                .top_level_target_metrics
                .iter()
                .map(|m| m.as_proto())
                .collect(),
            all_targets_build_metrics: Some(self.all_targets_build_metrics.as_proto()),
        }
    }
}

#[derive(Default)]
pub struct AggregatedBuildMetrics {
    pub full_graph_execution_time_ms: f64,
    pub full_graph_output_size_bytes: f64,

    pub local_execution_time_ms: f64,
    pub remote_execution_time_ms: f64,

    pub local_executions: f64,
    pub remote_executions: f64,
    pub remote_cache_hits: f64,

    pub analysis_retained_memory: f64,
    pub declared_actions: f64,
}

impl ToProtoMessage for AggregatedBuildMetrics {
    type Message = buck2_data::AggregatedBuildMetrics;

    fn as_proto(&self) -> Self::Message {
        buck2_data::AggregatedBuildMetrics {
            full_graph_execution_time_ms: self.full_graph_execution_time_ms,
            full_graph_output_size_bytes: self.full_graph_output_size_bytes,
            local_execution_time_ms: self.local_execution_time_ms,
            remote_execution_time_ms: self.remote_execution_time_ms,
            local_executions: self.local_executions,
            remote_executions: self.remote_executions,
            remote_cache_hits: self.remote_cache_hits,
            analysis_retained_memory: self.analysis_retained_memory,
            declared_actions: self.declared_actions,
        }
    }
}

pub struct TopLevelTargetAggregatedData {
    pub target: ConfiguredProvidersLabel,
    pub action_graph_size: Option<u64>,
    pub metrics: AggregatedBuildMetrics,
    pub amortized_metrics: AggregatedBuildMetrics,
    pub remote_max_memory_peak_bytes: u64,
    pub local_max_memory_peak_bytes: u64,
    /// Per-target action graph sketch for similarity comparison
    pub action_graph_sketch: Option<MergeableGraphSketch<ActionKey>>,
}

#[derive(Clone, Copy, Dupe)]
pub enum BuiltWhen {
    ThisBuild,
    Previously,
}

impl TopLevelTargetAggregatedData {
    pub fn new(
        target: ConfiguredProvidersLabel,
        action_graph_size: Option<usize>,
        action_graph_sketch: Option<MergeableGraphSketch<ActionKey>>,
    ) -> Self {
        Self {
            target,
            action_graph_size: action_graph_size.map(|v| v as u64),
            metrics: AggregatedBuildMetrics::default(),
            amortized_metrics: AggregatedBuildMetrics::default(),
            remote_max_memory_peak_bytes: 0,
            local_max_memory_peak_bytes: 0,
            action_graph_sketch,
        }
    }

    pub fn aggregate_execution_event(
        &mut self,
        factor: usize,
        ev: &ActionExecutionMetrics,
        when: BuiltWhen,
    ) {
        let factor = 1.0 / (factor as f64);
        self.metrics.aggregate_execution(1.0, ev, when);
        self.amortized_metrics.aggregate_execution(factor, ev, when);
    }

    pub fn aggregate_analysis_event(&mut self, factor: usize, ev: &AnalysisMetrics) {
        let factor = 1.0 / (factor as f64);
        self.metrics.aggregate_analysis(1.0, ev);
        self.amortized_metrics.aggregate_analysis(factor, ev);
    }

    pub fn aggregate_max_memory(&mut self, ev: &ActionExecutionMetrics) {
        let Some(memory_peak) = ev.memory_peak else {
            return;
        };
        match ev.execution_kind {
            ActionExecutionKind::Local | ActionExecutionKind::LocalWorker => {
                self.local_max_memory_peak_bytes =
                    max(self.local_max_memory_peak_bytes, memory_peak);
            }
            ActionExecutionKind::Remote | ActionExecutionKind::RemoteWorker => {
                self.remote_max_memory_peak_bytes =
                    max(self.remote_max_memory_peak_bytes, memory_peak);
            }
            ActionExecutionKind::NotSet
            | ActionExecutionKind::ActionCache
            | ActionExecutionKind::Simple
            | ActionExecutionKind::Deferred
            | ActionExecutionKind::LocalDepFile
            | ActionExecutionKind::RemoteDepFileCache
            | ActionExecutionKind::LocalActionCache => {
                // ignore
            }
        }
    }
}

impl ToProtoMessage for TopLevelTargetAggregatedData {
    type Message = buck2_data::TopLevelTargetMetrics;

    fn as_proto(&self) -> Self::Message {
        buck2_data::TopLevelTargetMetrics {
            target: Some(self.target.target().as_proto()),
            provider: match self.target.name() {
                ProvidersName::Default => None,
                v => Some(v.to_string()),
            },
            action_graph_size: self.action_graph_size,
            metrics: Some(self.metrics.as_proto()),
            amortized_metrics: Some(self.amortized_metrics.as_proto()),
            remote_max_memory_peak_bytes: Some(self.remote_max_memory_peak_bytes),
            local_max_memory_peak_bytes: Some(self.local_max_memory_peak_bytes),
        }
    }
}

impl AggregatedBuildMetrics {
    fn aggregate_execution(&mut self, factor: f64, ev: &ActionExecutionMetrics, when: BuiltWhen) {
        // Accumulate metrics computed over the full graph.
        self.full_graph_execution_time_ms += factor * (ev.execution_time_ms as f64);
        self.full_graph_output_size_bytes += factor * (ev.output_size_bytes as f64);

        if let BuiltWhen::ThisBuild = when {
            // Accumulate metrics associated with costs during this build.
            match ev.execution_kind {
                buck2_data::ActionExecutionKind::Local
                | buck2_data::ActionExecutionKind::LocalWorker => {
                    self.local_execution_time_ms += factor * (ev.execution_time_ms as f64);
                    self.local_executions += factor;
                }
                buck2_data::ActionExecutionKind::Remote
                | buck2_data::ActionExecutionKind::RemoteWorker => {
                    self.remote_execution_time_ms += factor * (ev.execution_time_ms as f64);
                    self.remote_executions += factor;
                }
                buck2_data::ActionExecutionKind::ActionCache
                | buck2_data::ActionExecutionKind::RemoteDepFileCache => {
                    self.remote_cache_hits += factor;
                }
                buck2_data::ActionExecutionKind::NotSet
                | buck2_data::ActionExecutionKind::Simple
                | buck2_data::ActionExecutionKind::Deferred
                | buck2_data::ActionExecutionKind::LocalDepFile
                | buck2_data::ActionExecutionKind::LocalActionCache => {
                    // ignored.
                }
            }
        }
    }

    fn aggregate_analysis(&mut self, factor: f64, ev: &AnalysisMetrics) {
        self.analysis_retained_memory += factor * (ev.retained_memory as f64);
        self.declared_actions += factor * (ev.actions as f64);
    }
}

#[derive(Default)]
pub struct AllTargetsAggregatedData {
    pub metrics: AggregatedBuildMetrics,
    pub action_graph_size: Option<u64>,
    pub compute_time_ms: Option<u64>,
}

impl AllTargetsAggregatedData {
    pub fn new(action_graph_size: Option<usize>) -> Self {
        Self {
            metrics: AggregatedBuildMetrics::default(),
            action_graph_size: action_graph_size.map(|v| v as u64),
            compute_time_ms: None,
        }
    }

    pub(crate) fn aggregate_execution_event(
        &mut self,
        ev: &ActionExecutionMetrics,
        built_when: BuiltWhen,
    ) {
        self.metrics.aggregate_execution(1.0, ev, built_when);
    }

    pub(crate) fn aggregate_analysis_event(&mut self, ev: &AnalysisMetrics) {
        self.metrics.aggregate_analysis(1.0, ev);
    }

    pub(crate) fn set_compute_time(&mut self, elapsed: std::time::Duration) {
        self.compute_time_ms = Some(elapsed.as_millis() as u64);
    }
}

impl ToProtoMessage for AllTargetsAggregatedData {
    type Message = buck2_data::AllTargetsBuildMetrics;

    fn as_proto(&self) -> Self::Message {
        buck2_data::AllTargetsBuildMetrics {
            action_graph_size: self.action_graph_size,
            metrics: Some(self.metrics.as_proto()),
            compute_time_ms: self.compute_time_ms,
        }
    }
}
