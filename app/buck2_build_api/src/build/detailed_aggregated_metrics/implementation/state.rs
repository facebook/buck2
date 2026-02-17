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
use std::time::Instant;

use buck2_artifact::actions::key::ActionKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_error::internal_error;
use dupe::Dupe;
use fxhash::FxHashSet;

use crate::build::detailed_aggregated_metrics::FxMultiMap;
use crate::build::detailed_aggregated_metrics::buck2_sketches::compute_action_graph_sketch;
use crate::build::detailed_aggregated_metrics::events::DetailedAggregatedMetricsEvent;
use crate::build::detailed_aggregated_metrics::events::DetailedAggregatedMetricsEventHandler;
use crate::build::detailed_aggregated_metrics::implementation::traverse::traverse_partial_action_graph;
use crate::build::detailed_aggregated_metrics::implementation::traverse::traverse_target_graph;
use crate::build::detailed_aggregated_metrics::types::ActionExecutionMetrics;
use crate::build::detailed_aggregated_metrics::types::AllTargetsAggregatedData;
use crate::build::detailed_aggregated_metrics::types::AnalysisMetrics;
use crate::build::detailed_aggregated_metrics::types::BuiltWhen;
use crate::build::detailed_aggregated_metrics::types::DetailedAggregatedMetrics;
use crate::build::detailed_aggregated_metrics::types::PerBuildEvents;
use crate::build::detailed_aggregated_metrics::types::TopLevelTargetAggregatedData;
use crate::build::graph_properties::GraphPropertiesOptions;
use crate::build::sketch_impl::DEFAULT_SKETCH_VERSION;
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
    analysis_nodes: Arc<fxhash::FxHashMap<DeferredHolderKey, DeferredHolder>>,
}

impl DetailedAggregatedMetricsStateTracker {
    pub(crate) fn start() -> DetailedAggregatedMetricsEventHandler {
        let (event_handler, mut event_receiver) = DetailedAggregatedMetricsEventHandler::new();

        tokio::task::spawn(async move {
            let mut state = Self::new();
            // This event loop cannot process events in parallel:
            //  - There is a ordering dependency between events
            //    (e.g. AnalysisStarted cannot be run in parallel with AnalysisComplete)
            //  - The state is mutated by events and would either need to be cloned
            //    or be protected by a mutex. Cloning is expensive and a mutex would defeat parallelism.
            while let Some(v) = event_receiver.recv().await {
                state.event(v).await;
            }
        });

        event_handler
    }

    fn new() -> Self {
        Self {
            analysis_nodes: Arc::new(fxhash::FxHashMap::default()),
            observed_executions: fxhash::FxHashMap::default(),
        }
    }

    pub(crate) async fn event(&mut self, ev: DetailedAggregatedMetricsEvent) {
        let analysis_nodes = Arc::get_mut(&mut self.analysis_nodes)
            .expect("Metrics state should have a single reference");
        match ev {
            DetailedAggregatedMetricsEvent::AnalysisStarted(key) => {
                analysis_nodes.remove(&key);
            }
            DetailedAggregatedMetricsEvent::AnalysisComplete(key, data) => {
                analysis_nodes.insert(key, data);
            }
            DetailedAggregatedMetricsEvent::ComputeMetrics(events, graph_properties, sender) => {
                drop(sender.send(self.compute_metrics(events, graph_properties).await))
            }
            DetailedAggregatedMetricsEvent::ComputeActionGraphSketch(top_level_targets, sender) => {
                drop(sender.send(self.compute_action_graph_sketch_only(&top_level_targets)))
            }
            DetailedAggregatedMetricsEvent::ActionExecuted(metrics) => {
                self.observed_executions.insert(metrics.key.dupe(), metrics);
            }
        }
    }

    async fn compute_metrics(
        &self,
        events: PerBuildEvents,
        graph_properties: GraphPropertiesOptions,
    ) -> buck2_error::Result<DetailedAggregatedMetrics> {
        let now = Instant::now();

        let compute_sketches = graph_properties.action_graph_sketch;

        let futures = events
            .top_level_targets
            .into_iter()
            .enumerate()
            .map(|(idx, spec)| {
                let analysis_nodes = self.analysis_nodes.dupe();
                tokio::task::spawn_blocking(move || {
                    let mut target_graph = FxHashSet::default();
                    traverse_target_graph(&spec.target, |target| {
                        target_graph.insert(target.dupe());
                    });
                    let action_graph_result = traverse_partial_action_graph(
                        spec.outputs.iter().map(|(artifact, _)| artifact),
                        &analysis_nodes,
                    );

                    // Compute per-target action graph sketch if requested
                    let per_target_sketch = if compute_sketches {
                        match compute_action_graph_sketch(
                            spec.outputs.iter().map(|(artifact, _)| artifact),
                            &analysis_nodes,
                        ) {
                            Ok((_complete, sketch)) if !sketch.is_empty() => Some(sketch),
                            _ => None,
                        }
                    } else {
                        None
                    };

                    (
                        idx,
                        spec.label,
                        target_graph,
                        action_graph_result,
                        per_target_sketch,
                    )
                })
            });

        let results = buck2_util::future::try_join_all(futures).await?;

        let mut action_mappings: FxMultiMap<ActionKey, usize> = FxMultiMap::default();
        let mut target_mappings: FxMultiMap<ConfiguredTargetLabel, usize> = FxMultiMap::default();
        let mut all_complete = true;
        let mut agg_data = Vec::new();

        // Create a sketcher to merge all per-target sketches into an overall sketch
        let mut overall_sketcher = if compute_sketches {
            Some(DEFAULT_SKETCH_VERSION.create_sketcher())
        } else {
            None
        };

        for (idx, label, target_graph, action_graph_result, per_target_sketch) in results {
            for target in target_graph {
                target_mappings.insert(target, idx);
            }
            let (action_graph_complete, action_graph) = action_graph_result?;

            // Merge per-target sketch into overall sketch
            if let (Some(sketcher), Some(sketch)) = (&mut overall_sketcher, &per_target_sketch) {
                sketcher.merge(sketch)?;
            }

            agg_data.push(TopLevelTargetAggregatedData::new(
                label,
                if action_graph_complete {
                    Some(action_graph.len())
                } else {
                    all_complete = false;
                    None
                },
                per_target_sketch,
            ));
            for action in action_graph {
                action_mappings.insert(action, idx);
            }
        }

        let action_graph_sketch = overall_sketcher
            .map(|s| s.into_mergeable_graph_sketch())
            .filter(|s| !s.is_empty());

        let mut all_targets_data = AllTargetsAggregatedData::new(if all_complete {
            Some(action_mappings.len())
        } else {
            None
        });

        for (action, owners) in action_mappings.into_iter() {
            let built_when = if events.executed_actions.contains(&action) {
                BuiltWhen::ThisBuild
            } else {
                BuiltWhen::Previously
            };
            if let Some(ev) = self.observed_executions.get(&action) {
                all_targets_data.aggregate_execution_event(ev, built_when);
                let amortization_factor = owners.len();
                for idx in owners {
                    agg_data[idx].aggregate_execution_event(amortization_factor, ev, built_when);
                    agg_data[idx].aggregate_max_memory(ev);
                }
            }
        }

        for (target, owners) in target_mappings.into_iter() {
            let ev = self
                .analysis_nodes
                .get(&DeferredHolderKey::for_analysis(target.dupe()))
                .ok_or_else(|| internal_error!("analysis missing for output"))?;
            let metrics = AnalysisMetrics {
                actions: ev.analysis_values().iter_actions().count(),
                retained_memory: ev.analysis_values().retained_memory().expect("todo!()"),
            };
            let amortization_factor = owners.len();
            all_targets_data.aggregate_analysis_event(&metrics);
            for idx in owners {
                agg_data[idx].aggregate_analysis_event(amortization_factor, &metrics);
            }
        }

        all_targets_data.set_compute_time(Instant::now() - now);

        Ok(DetailedAggregatedMetrics {
            all_targets_build_metrics: all_targets_data,
            top_level_target_metrics: agg_data,
            action_graph_sketch,
        })
    }

    fn compute_action_graph_sketch_only(
        &self,
        top_level_targets: &[crate::build::detailed_aggregated_metrics::types::TopLevelTargetSpec],
    ) -> buck2_error::Result<Option<crate::build::sketch_impl::MergeableGraphSketch<ActionKey>>>
    {
        if top_level_targets.is_empty() {
            return Ok(None);
        }
        let root_artifacts: Vec<_> = top_level_targets
            .iter()
            .flat_map(|spec| spec.outputs.iter().map(|(artifact, _)| artifact))
            .collect();
        if root_artifacts.is_empty() {
            return Ok(None);
        }
        let (_complete, sketch) =
            compute_action_graph_sketch(root_artifacts.iter().copied(), &self.analysis_nodes)?;
        if sketch.is_empty() {
            Ok(None)
        } else {
            Ok(Some(sketch))
        }
    }
}
