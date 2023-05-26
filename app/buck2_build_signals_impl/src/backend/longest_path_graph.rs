/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Duration;

use anyhow::Context as _;
use buck2_build_api::actions::RegisteredAction;
use buck2_build_api::build_signals::CriticalPathBackendName;
use buck2_build_api::build_signals::NodeDuration;
use buck2_core::soft_error;
use buck2_critical_path::compute_critical_path_potentials;
use buck2_critical_path::GraphBuilder;
use buck2_critical_path::OptionalVertexId;
use buck2_critical_path::PushError;
use buck2_events::span::SpanId;
use dupe::Dupe;
use smallvec::SmallVec;

use crate::backend::backend::BuildListenerBackend;
use crate::BuildInfo;
use crate::NodeData;
use crate::NodeKey;

/// An implementation of critical path that uses a longest-paths graph in order to produce
/// potential savings in addition to the critical path.
pub(crate) struct LongestPathGraphBackend {
    builder: anyhow::Result<GraphBuilder<NodeKey, NodeData>>,
    top_level_analysis: Vec<VisibilityEdge>,
}

/// Represents nodes that block us "seeing" other parts of the graph until they finish evaluating.
struct VisibilityEdge {
    node: NodeKey,
    makes_visible: Vec<NodeKey>,
}

impl LongestPathGraphBackend {
    pub(crate) fn new() -> Self {
        Self {
            builder: Ok(GraphBuilder::new()),
            top_level_analysis: Vec::new(),
        }
    }
}

impl BuildListenerBackend for LongestPathGraphBackend {
    fn process_node(
        &mut self,
        key: NodeKey,
        action: Option<Arc<RegisteredAction>>,
        duration: NodeDuration,
        dep_keys: impl Iterator<Item = NodeKey>,
        span_ids: SmallVec<[SpanId; 1]>,
    ) {
        let builder = match self.builder.as_mut() {
            Ok(b) => b,
            Err(..) => return,
        };

        let res = builder.push(
            key,
            dep_keys,
            NodeData {
                action,
                duration,
                span_ids,
            },
        );

        let res = res.or_else(|err| match err {
            e @ PushError::Overflow => Err(e.into()),
            e @ PushError::DuplicateKey { .. } => {
                soft_error!("critical_path_duplicate_key", e.into(), quiet: true)?;
                anyhow::Ok(())
            }
        });

        match res {
            Ok(()) => {}
            Err(e) => self.builder = Err(e),
        }
    }

    fn process_top_level_target(
        &mut self,
        analysis: NodeKey,
        artifacts: impl Iterator<Item = NodeKey>,
    ) {
        self.top_level_analysis.push(VisibilityEdge {
            node: analysis,
            makes_visible: artifacts.collect(),
        })
    }

    fn finish(self) -> anyhow::Result<BuildInfo> {
        let (graph, keys, mut data) = {
            let (graph, keys, data) = self.builder?.finish();

            let mut first_analysis = graph.allocate_vertex_data(OptionalVertexId::none());
            let mut n = 0;

            for visibility in &self.top_level_analysis {
                let analysis = &visibility.node;
                let artifacts = &visibility.makes_visible;

                let analysis = match keys.get(analysis) {
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
                .context("Error adding first_analysis edges to graph")?;

            (graph, keys, data)
        };

        let durations = data.try_map_ref(|d| {
            d.duration
                .critical_path_duration()
                .as_micros()
                .try_into()
                .context("Duration `as_micros()` exceeds u64")
        })?;

        let (critical_path, critical_path_cost, replacement_durations) =
            compute_critical_path_potentials(&graph, &durations)
                .context("Error computing critical path potentials")?;

        drop(durations);

        let critical_path = critical_path
            .iter()
            .map(|(cp_idx, vertex_idx)| {
                let vertex_idx = *vertex_idx;
                let key = keys[vertex_idx].dupe();

                // OK to replace `data` with empty things here because we know that we will not access
                // the same index twice.
                let data = std::mem::replace(
                    &mut data[vertex_idx],
                    NodeData {
                        action: None,
                        duration: NodeDuration::zero(),
                        span_ids: Default::default(),
                    },
                );

                let potential = critical_path_cost.runtime - replacement_durations[cp_idx].runtime;

                (key, data, Some(Duration::from_micros(potential)))
            })
            .collect();

        Ok(BuildInfo {
            critical_path,
            num_nodes: graph.vertices_count() as _,
            num_edges: graph.edges_count() as _,
        })
    }

    fn name() -> CriticalPathBackendName {
        CriticalPathBackendName::LongestPathGraph
    }
}
