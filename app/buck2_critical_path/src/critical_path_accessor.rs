/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::graph::PathCost;
use crate::CriticalPathVertexData;
use crate::OptionalVertexId;
use crate::VertexData;
use crate::VertexId;

/// A container that can be used to access critical path from arbitrary vertices.
///
/// When constructed, this must maintain the invariants that predecessors and
/// cost_from_source are from the same graph (thus ensuring that if the path
/// tells us a node has n predecessors, then we can actually find n-non-null
/// entries when following the path in the predecessor graph).
pub struct CriticalPathAccessor {
    pub(crate) predecessors: VertexData<OptionalVertexId>,
    pub(crate) cost_from_source: VertexData<PathCost>,
}

impl CriticalPathAccessor {
    /// Get the critical path for a given vertex. Passing an invalid vertex ID will return None.
    pub fn critical_path_for_vertex(
        &self,
        sink: VertexId,
    ) -> Option<(PathCost, CriticalPathVertexData<VertexId>)> {
        let cost = *self.cost_from_source.get(sink)?;
        let critical_path = self.critical_path_for_vertex_and_cost(sink, cost);
        Some((cost, critical_path))
    }

    pub(crate) fn critical_path_for_vertex_and_cost(
        &self,
        sink: VertexId,
        cost: PathCost,
    ) -> CriticalPathVertexData<VertexId> {
        let critical_path_len = cost.len as usize;
        let mut critical_path = vec![VertexId::new(0); critical_path_len];
        let mut idx: VertexId = sink;
        for i in 0..critical_path_len {
            critical_path[critical_path_len - 1 - i] = idx;
            if i != critical_path_len - 1 {
                // Unwrap safety: we know that the predecessor cannot be null
                // because the critical path length tells us how many
                // predecessors we expect.
                idx = self.predecessors[idx].into_option().unwrap();
            }
        }
        CriticalPathVertexData::new(critical_path)
    }
}
