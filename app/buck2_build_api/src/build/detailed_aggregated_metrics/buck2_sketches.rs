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

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use fxhash::FxHashSet;
use starlark::values::FrozenHeapRef;

use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::build::graph_properties::ConfiguredGraphPropertiesValues;
use crate::build::sketch_impl::DEFAULT_SKETCH_VERSION;
use crate::build::sketch_impl::MergeableGraphSketch;

/// Returns the total graph size for all dependencies of a target without caching the result on the DICE graph.
/// The cost of storing this on DICE is extremely low, so there's almost no reason to use this function (we currently
/// expose it just for performance testing).
pub fn compute_configured_graph_sketch(
    node: ConfiguredTargetNode,
    configured_graph_sketch: bool,
) -> ConfiguredGraphPropertiesValues {
    let mut queue = vec![&node];
    let mut visited: HashSet<_, fxhash::FxBuildHasher> = HashSet::default();
    visited.insert(&node);

    let mut configured_graph_sketch = if configured_graph_sketch {
        Some(DEFAULT_SKETCH_VERSION.create_sketcher())
    } else {
        None
    };

    while let Some(item) = queue.pop() {
        for d in item.deps() {
            if visited.insert(d) {
                queue.push(d);
            }
        }

        if let Some(sketch) = configured_graph_sketch.as_mut() {
            sketch.sketch(item.label());
        }
    }

    ConfiguredGraphPropertiesValues {
        configured_graph_size: visited.len() as _,
        configured_graph_sketch: configured_graph_sketch
            .map(|sketch| sketch.into_mergeable_graph_sketch()),
    }
}

#[derive(
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Allocative
)]
#[display("AnalysisGraphPropertiesKey({})", label)]
pub(crate) struct AnalysisGraphPropertiesKey {
    pub label: ConfiguredTargetLabel,
}

#[async_trait]
impl Key for AnalysisGraphPropertiesKey {
    type Value = buck2_error::Result<MaybeCompatible<MergeableGraphSketch<StarlarkEvalKind>>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let analysis_result = ctx.get_analysis_result(&self.label).await?;
        analysis_result.try_map(|analysis_result| {
            Ok(gather_heap_graph_sketch(
                analysis_result
                    .analysis_values()
                    .analysis_storage()?
                    .owner(),
            ))
        })
    }

    fn equality(a: &Self::Value, b: &Self::Value) -> bool {
        match (a, b) {
            (Ok(a), Ok(b)) => a == b,
            _ => false,
        }
    }
}

/// Computes a sketch of the memory transitively referenced by the given starlark heap.
///
/// Using the heap graph instead of the configured graph directly is advantageous primarily because
/// analysis results don't otherwise directly encode the structure of the analysis graph. "Guessing"
/// at what the graph is, while probably possible in practice, is a bit brittle. It would also mean
/// that we wouldn't know about anon targets, which would be a bit of a shame.
fn gather_heap_graph_sketch(root: &FrozenHeapRef) -> MergeableGraphSketch<StarlarkEvalKind> {
    let mut visited = FxHashSet::default();
    visited.insert(root);
    let mut queue = vec![root];
    let mut sketcher = DEFAULT_SKETCH_VERSION.create_sketcher();

    while let Some(item) = queue.pop() {
        let Some(name) = item.name() else {
            continue;
        };
        let name = name.downcast_ref::<StarlarkEvalKind>().unwrap();
        sketcher.sketch_weighted(name, item.allocated_bytes() as u64);
        queue.extend(item.refs().filter(|f| visited.insert(*f)));
    }

    sketcher.into_mergeable_graph_sketch()
}
