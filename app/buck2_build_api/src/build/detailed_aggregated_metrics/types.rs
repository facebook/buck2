/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_artifact::actions::key::ActionKey;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_data::ToProtoMessage;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dupe::Dupe;

use crate::artifact_groups::ArtifactGroup;
use crate::build::BuildProviderType;

#[derive(Clone)]
pub struct ActionExecutionMetrics {
    pub key: ActionKey,
    pub execution_time_ms: u64,
    pub execution_kind: buck2_data::ActionExecutionKind,
    pub output_size_bytes: u64,
}

pub struct AnalysisMetrics {
    pub actions: usize,
    pub retained_memory: usize,
}

pub struct TopLevelTargetSpec {
    pub label: Arc<ConfiguredProvidersLabel>,
    pub target: ConfiguredTargetNode,
    pub outputs: Arc<Vec<(ArtifactGroup, BuildProviderType)>>,
}

#[derive(Default)]
pub struct PerBuildEvents {
    pub executed_actions: fxhash::FxHashSet<ActionKey>,
    pub top_level_targets: Vec<TopLevelTargetSpec>,
}

pub struct TopLevelTargetAggregatedData {
    proto: buck2_data::TopLevelTargetMetrics,
}

#[derive(Clone, Copy, Dupe)]
pub enum BuiltWhen {
    ThisBuild,
    Previously,
}

impl TopLevelTargetAggregatedData {
    pub fn new(target: &ConfiguredProvidersLabel, action_graph_size: Option<usize>) -> Self {
        Self {
            proto: buck2_data::TopLevelTargetMetrics {
                target: Some(target.target().as_proto()),
                provider: match target.name() {
                    ProvidersName::Default => None,
                    v => Some(v.to_string()),
                },
                action_graph_size: action_graph_size.map(|v| v as u64),
                metrics: Some(buck2_data::AggregatedBuildMetrics::default()),
                amortized_metrics: Some(buck2_data::AggregatedBuildMetrics::default()),
            },
        }
    }

    pub fn aggregate_execution_event(
        &mut self,
        factor: usize,
        ev: &ActionExecutionMetrics,
        when: BuiltWhen,
    ) {
        let factor = 1.0 / (factor as f64);
        self.proto
            .metrics
            .as_mut()
            .unwrap()
            .aggregate_execution(1.0, ev, when);
        self.proto
            .amortized_metrics
            .as_mut()
            .unwrap()
            .aggregate_execution(factor, ev, when);
    }

    pub fn aggregate_analysis_event(&mut self, factor: usize, ev: &AnalysisMetrics) {
        let factor = 1.0 / (factor as f64);
        self.proto
            .metrics
            .as_mut()
            .unwrap()
            .aggregate_analysis(1.0, ev);
        self.proto
            .amortized_metrics
            .as_mut()
            .unwrap()
            .aggregate_analysis(factor, ev);
    }

    pub fn into_proto(self) -> buck2_data::TopLevelTargetMetrics {
        self.proto
    }
}

trait AggregatedBuildMetricsExt {
    fn aggregate_execution(&mut self, factor: f64, ev: &ActionExecutionMetrics, when: BuiltWhen);
    fn aggregate_analysis(&mut self, factor: f64, ev: &AnalysisMetrics);
}

impl AggregatedBuildMetricsExt for buck2_data::AggregatedBuildMetrics {
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
                buck2_data::ActionExecutionKind::Remote => {
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

pub struct AllTargetsAggregatedData {
    pub proto: buck2_data::AllTargetsBuildMetrics,
}

impl AllTargetsAggregatedData {
    pub fn new(action_graph_size: Option<usize>) -> Self {
        Self {
            proto: buck2_data::AllTargetsBuildMetrics {
                action_graph_size: action_graph_size.map(|v| v as u64),
                metrics: Some(buck2_data::AggregatedBuildMetrics::default()),
                compute_time_ms: None,
            },
        }
    }

    pub(crate) fn aggregate_execution_event(
        &mut self,
        ev: &ActionExecutionMetrics,
        built_when: BuiltWhen,
    ) {
        self.proto
            .metrics
            .as_mut()
            .unwrap()
            .aggregate_execution(1.0, ev, built_when);
    }

    pub fn into_proto(self) -> buck2_data::AllTargetsBuildMetrics {
        self.proto
    }

    pub(crate) fn aggregate_analysis_event(&mut self, ev: &AnalysisMetrics) {
        self.proto
            .metrics
            .as_mut()
            .unwrap()
            .aggregate_analysis(1.0, ev);
    }

    pub(crate) fn set_compute_time(&mut self, elapsed: std::time::Duration) {
        self.proto.compute_time_ms = Some(elapsed.as_millis() as u64);
    }
}
