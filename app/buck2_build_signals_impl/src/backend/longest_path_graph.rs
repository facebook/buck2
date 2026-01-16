/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;

use buck2_analysis::analysis::calculation::AnalysisKey;
use buck2_build_signals::env::CriticalPathBackendName;
use buck2_build_signals::env::NodeDuration;
use buck2_build_signals::env::WaitingData;
use buck2_core::soft_error;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_critical_path::Graph;
use buck2_critical_path::GraphBuilder;
use buck2_critical_path::OptionalVertexId;
use buck2_critical_path::PushError;
use buck2_critical_path::VertexData;
use buck2_critical_path::VertexKeys;
use buck2_critical_path::compute_critical_path_potentials;
use buck2_error::BuckErrorContext;
use buck2_events::span::SpanId;
use dupe::Dupe;
use smallvec::SmallVec;

use crate::BuildInfo;
use crate::DetailedCriticalPath;
use crate::DetailedCriticalPathEntry;
use crate::NodeData;
use crate::NodeExtraData;
use crate::NodeKey;
use crate::backend::backend::BuildListenerBackend;

/// An implementation of critical path that uses a longest-paths graph in order to produce
/// potential savings in addition to the critical path.
pub(crate) struct LongestPathGraphBackend {
    builder: buck2_error::Result<GraphBuilder<NodeKey, NodeData>>,
    top_level_targets: Vec<TopLevelTarget>,
}

/// Represents nodes that block us "seeing" other parts of the graph until they finish evaluating.
struct TopLevelTarget {
    target: ConfiguredTargetLabel,
    artifacts: Vec<NodeKey>,
}

impl LongestPathGraphBackend {
    pub(crate) fn new() -> Self {
        Self {
            builder: Ok(GraphBuilder::new()),
            top_level_targets: Vec::new(),
        }
    }
}

impl BuildListenerBackend for LongestPathGraphBackend {
    fn process_node(
        &mut self,
        key: NodeKey,
        extra_data: NodeExtraData,
        duration: NodeDuration,
        dep_keys: impl IntoIterator<Item = NodeKey>,
        span_ids: SmallVec<[SpanId; 1]>,
        waiting_data: WaitingData,
    ) {
        let builder = match self.builder.as_mut() {
            Ok(b) => b,
            Err(..) => return,
        };

        let res = builder.push(
            key,
            dep_keys,
            NodeData {
                extra_data,
                duration,
                span_ids,
                waiting_data,
            },
        );

        let res = res.or_else(|err| match err {
            e @ PushError::Overflow => Err(e.into()),
            e @ PushError::DuplicateKey { .. } => {
                soft_error!("critical_path_duplicate_key", e.into(), quiet: true)?;
                Ok(())
            }
        });

        match res {
            Ok(()) => {}
            Err(e) => self.builder = Err(e),
        }
    }

    fn process_top_level_target(
        &mut self,
        target: ConfiguredTargetLabel,
        artifacts: impl IntoIterator<Item = NodeKey>,
    ) {
        self.top_level_targets.push(TopLevelTarget {
            target,
            artifacts: artifacts.into_iter().collect(),
        })
    }

    fn finish(self) -> buck2_error::Result<BuildInfo> {
        let (graph, keys, data) = {
            let (graph, keys, data) = self.builder?.finish();

            let mut first_analysis = graph.allocate_vertex_data(OptionalVertexId::none());
            let mut n = 0;

            for top_level_target in &self.top_level_targets {
                // This is a bit wasteful, but transient and the volume is small.
                let analysis = NodeKey::AnalysisKey(AnalysisKey(top_level_target.target.dupe()));
                let artifacts = &top_level_target.artifacts;

                let analysis = match keys.get(&analysis) {
                    Some(k) => k,
                    None => continue, // Nothing depends on this,
                };

                let mut queue = Vec::new();

                // We have an analysis and a set of artifacts that we decided to build after having
                // evaluated this analysis. So, traverse all of those artifacts' dependencies, and
                // label them as depending on this top level analysis (but we only do that once).
                // Concretely, this expresses the idea that we only started knowing we cared about
                // those artifacts once we finished that analysis.

                for artifact in artifacts {
                    let artifact = match keys.get(artifact) {
                        Some(a) => a,
                        None => {
                            // Not built. Unexpected, but we don't report signals in all failure cases so that can happen.
                            continue;
                        }
                    };

                    queue.push(artifact);

                    while let Some(i) = queue.pop() {
                        if first_analysis[i].is_some() {
                            continue;
                        }

                        // We only traverse edges to things that produce artifacts here.
                        match keys[i] {
                            NodeKey::BuildKey(..)
                            | NodeKey::EnsureTransitiveSetProjectionKey(..)
                            | NodeKey::EnsureProjectedArtifactKey(..) => {}
                            _ => {
                                continue;
                            }
                        };

                        first_analysis[i] = analysis.into();
                        queue.extend(graph.iter_edges(i));
                        n += 1;
                    }
                }
            }

            let graph = graph
                .add_edges(&first_analysis, n)
                .buck_error_context("Error adding first_analysis edges to graph")?;

            (graph, keys, data)
        };

        let durations = data.try_map_ref(|d| {
            d.duration
                .critical_path_duration()
                .as_micros()
                .try_into()
                .buck_error_context("Duration `as_micros()` exceeds u64")
        })?;

        let (slowest_path, critical_path, critical_path_for_top_level_targets) =
            std::thread::scope(|s| {
                let cp = s.spawn(|| {
                    compute_critical_paths(&graph, &keys, &data, durations, &self.top_level_targets)
                });
                let slow = s.spawn(|| compute_slowest_paths(&graph, &keys, &data));

                let (cp, cp_top) = cp.join().expect("thread panicked")?;
                let slow = slow.join().expect("thread panicked")?;

                buck2_error::Ok((slow, cp, cp_top))
            })?;

        Ok(BuildInfo {
            critical_path,
            slowest_path,
            num_nodes: graph.vertices_count() as _,
            num_edges: graph.edges_count() as _,
            top_level_targets: critical_path_for_top_level_targets,
        })
    }

    fn name() -> CriticalPathBackendName {
        CriticalPathBackendName::LongestPathGraph
    }
}

fn compute_critical_paths(
    graph: &Graph,
    keys: &VertexKeys<NodeKey>,
    data: &VertexData<NodeData>,
    durations: VertexData<u64>,
    top_level_targets: &[TopLevelTarget],
) -> buck2_error::Result<(DetailedCriticalPath, Vec<(ConfiguredTargetLabel, Duration)>)> {
    let (critical_path, critical_path_cost, replacement_durations, critical_path_accessor) =
        compute_critical_path_potentials(&graph, &durations)
            .buck_error_context("Error computing critical path potentials")?;

    let critical_path_for_top_level_targets = top_level_targets
        .iter()
        .filter_map(|t| {
            let max_cost = (|| {
                let (path_cost, _critical_path) = t
                    .artifacts
                    .iter()
                    .map(|a| {
                        let idx = keys
                            .get(a)
                            .with_buck_error_context(|| format!("Cannot find artifact: {a}"))?;
                        critical_path_accessor
                            .critical_path_for_vertex(idx)
                            .with_buck_error_context(|| format!("Invalid index for artifact: {a}"))
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .max_by_key(|p| p.0)
                    .buck_error_context("No critical path")?;

                buck2_error::Result::Ok(Duration::from_micros(path_cost.runtime))
            })();

            let max_cost = match max_cost {
                Ok(max_cost) => max_cost,
                Err(e) => {
                    // This would happen if we're given a target at the top
                    // level but then its DICE node never gets computed.
                    // This may happen if the command was e.g. cancelled.
                    tracing::debug!("No critical path for target {}: {:#}", t.target, e);
                    return None;
                }
            };

            Some((t.target.dupe(), max_cost))
        })
        .collect::<Vec<_>>();

    drop(durations);

    let critical_path = critical_path
        .iter()
        .map(|(cp_idx, vertex_idx)| {
            let vertex_idx = *vertex_idx;
            let key = keys[vertex_idx].dupe();

            let node_data = data[vertex_idx].clone();
            let deps_finished_time = graph
                .iter_edges(vertex_idx)
                .map(|d| data[d].duration.total.end())
                .max();
            let potential_improvement = Some(Duration::from_micros(
                critical_path_cost.runtime - replacement_durations[cp_idx].runtime,
            ));
            DetailedCriticalPathEntry {
                key,
                data: node_data,
                potential_improvement,
                deps_finished_time,
            }
        })
        .collect();

    Ok((
        DetailedCriticalPath::new(critical_path),
        critical_path_for_top_level_targets,
    ))
}

/// Computes the "slowest path" where each node's predecessor is the dependency that finished last.
/// This differs from critical path where predecessors have the greatest critical path length.
/// The slowest path makes waiting time directly attributable to what a node is immediately waiting on.
fn compute_slowest_paths(
    graph: &Graph,
    keys: &VertexKeys<NodeKey>,
    data: &VertexData<NodeData>,
) -> buck2_error::Result<DetailedCriticalPath> {
    let mut last = None;

    for d in graph.iter_vertices() {
        let d_end = data[d].duration.total.end();
        if last.is_none_or(|(l_end, _)| l_end < d_end) {
            last = Some((d_end, d));
        }
    }

    let mut slowest_path = Vec::new();
    let mut node = last.unzip().1;
    while let Some(curr) = node {
        let mut prev = None;
        for d in graph.iter_edges(curr) {
            let d_end = data[d].duration.total.end();
            if prev.is_none_or(|(p_end, _)| p_end < d_end) {
                prev = Some((d_end, d));
            }
        }

        let (deps_finished_time, prev_node) = prev.unzip();

        slowest_path.push(DetailedCriticalPathEntry {
            key: keys[curr].dupe(),
            data: data[curr].clone(),
            potential_improvement: None,
            deps_finished_time,
        });
        node = prev_node;
    }

    slowest_path.reverse();

    Ok(DetailedCriticalPath::new(slowest_path))
}
