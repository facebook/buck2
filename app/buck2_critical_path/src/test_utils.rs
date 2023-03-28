/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use rand::prelude::*;
use rand_chacha::ChaCha8Rng;
use rand_distr::Normal;

use crate::graph::Graph;
use crate::graph::GraphVertex;
use crate::types::VertexData;
use crate::types::VertexId;

#[derive(Clone)]
pub struct TestDag {
    pub graph: Graph,
    pub keys: VertexData<String>,
    pub runtimes: VertexData<u64>,
}

pub fn seeded_rng() -> ChaCha8Rng {
    ChaCha8Rng::seed_from_u64(123)
}

pub fn make_dag(nodes: usize, rng: &mut impl Rng) -> TestDag {
    let degree_distribution = Normal::<f64>::new(2.0, 5.0).unwrap();

    let mut keys = Vec::new();
    let mut runtimes = Vec::new();
    let mut vertices = Vec::new();
    let mut edges = Vec::new();

    for i in 0..nodes {
        let candidate_count = nodes - 1 - i;
        let edges_count = degree_distribution.sample(rng).round() as usize;
        let edges_count = edges_count.min(candidate_count);

        keys.push(format!("k{}", i));
        runtimes.push(rng.gen_range(0..10_000));
        vertices.push(GraphVertex {
            edges_idx: edges.len().try_into().unwrap(),
            edges_count: edges_count.try_into().unwrap(),
        });

        // Those are relative to i + 1.
        for j in rand::seq::index::sample(rng, candidate_count, edges_count) {
            let j = i + 1 + j;
            edges.push(VertexId::new(j.try_into().unwrap())); // i depends on j
        }
    }

    TestDag {
        keys: VertexData::new(keys),
        runtimes: VertexData::new(runtimes),
        graph: Graph {
            vertices: VertexData::new(vertices),
            edges,
        },
    }
}
