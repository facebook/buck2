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
use std::fmt;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_util::commas::commas;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::FutureExt;
use fxhash::FxHashSet;
use starlark::values::FrozenHeapRef;

use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::build::sketch_impl::DEFAULT_SKETCH_VERSION;
use crate::build::sketch_impl::MergeableGraphSketch;

#[derive(Copy, Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative, Default)]
pub struct GraphPropertiesOptions {
    pub configured_graph_size: bool,
    pub configured_graph_sketch: bool,
    pub total_configured_graph_sketch: bool,
    pub retained_analysis_memory_sketch: bool,
}

impl fmt::Display for GraphPropertiesOptions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            configured_graph_size,
            configured_graph_sketch,
            total_configured_graph_sketch,
            retained_analysis_memory_sketch,
        } = *self;

        let mut comma = commas();

        if configured_graph_size {
            comma(f)?;
            write!(f, "configured_graph_size")?;
        }

        if configured_graph_sketch {
            comma(f)?;
            write!(f, "configured_graph_sketch")?;
        }

        if total_configured_graph_sketch {
            comma(f)?;
            write!(f, "total_configured_graph_sketch")?;
        }

        if retained_analysis_memory_sketch {
            comma(f)?;
            write!(f, "retained_analysis_memory_sketch")?;
        }

        Ok(())
    }
}

impl GraphPropertiesOptions {
    pub fn is_empty(self) -> bool {
        let Self {
            configured_graph_size,
            configured_graph_sketch,
            total_configured_graph_sketch,
            retained_analysis_memory_sketch,
        } = self;

        !configured_graph_size
            && !configured_graph_sketch
            && !total_configured_graph_sketch
            && !retained_analysis_memory_sketch
    }

    pub(crate) fn should_compute_configured_graph_sketch(self) -> bool {
        self.configured_graph_sketch || self.total_configured_graph_sketch
    }
}

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Allocative)]
pub struct ConfiguredGraphPropertiesValues {
    pub configured_graph_size: u64,
    pub configured_graph_sketch: Option<MergeableGraphSketch<ConfiguredTargetLabel>>,
}

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Allocative)]
pub struct GraphPropertiesValues {
    pub configured: ConfiguredGraphPropertiesValues,
    pub retained_analysis_memory_sketch: Option<MergeableGraphSketch<StarlarkEvalKind>>,
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
#[display(
    "GraphPropertiesKey: {}, configured_graph_sketch={}",
    label,
    configured_graph_sketch
)]
struct ConfiguredGraphPropertiesKey {
    label: ConfiguredTargetLabel,
    configured_graph_sketch: bool,
}

#[async_trait]
impl Key for ConfiguredGraphPropertiesKey {
    type Value = buck2_error::Result<MaybeCompatible<ConfiguredGraphPropertiesValues>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let configured_node = ctx.get_configured_target_node(&self.label).await?;
        Ok(configured_node.map(|configured_node| {
            debug_compute_configured_graph_properties_uncached(
                configured_node,
                self.configured_graph_sketch,
            )
        }))
    }

    fn equality(a: &Self::Value, b: &Self::Value) -> bool {
        match (a, b) {
            (Ok(a), Ok(b)) => a == b,
            _ => false,
        }
    }
}

/// Returns the total graph size for all dependencies of a target.
pub async fn get_graph_properties(
    ctx: &mut DiceComputations<'_>,
    label: &ConfiguredTargetLabel,
    configured_graph_sketch: bool,
    retained_analysis_memory_sketch: bool,
) -> buck2_error::Result<MaybeCompatible<GraphPropertiesValues>> {
    let (conf, analysis) = ctx
        .try_compute2(
            |ctx| {
                async {
                    ctx.compute(&ConfiguredGraphPropertiesKey {
                        label: label.dupe(),
                        configured_graph_sketch,
                    })
                    .await?
                }
                .boxed()
            },
            |ctx| {
                async {
                    if retained_analysis_memory_sketch {
                        Ok(Some(
                            ctx.compute(&AnalysisGraphPropertiesKey {
                                label: label.dupe(),
                            })
                            .await??,
                        ))
                    } else {
                        Ok(None)
                    }
                }
                .boxed()
            },
        )
        .await?;
    Ok(conf.map(|conf| GraphPropertiesValues {
        configured: conf,
        retained_analysis_memory_sketch: analysis.map(|a| a.require_compatible().unwrap()),
    }))
}

/// Returns the total graph size for all dependencies of a target without caching the result on the DICE graph.
/// The cost of storing this on DICE is extremely low, so there's almost no reason to use this function (we currently
/// expose it just for performance testing).
pub fn debug_compute_configured_graph_properties_uncached(
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
#[display("GraphPropertiesKey({})", label)]
struct AnalysisGraphPropertiesKey {
    label: ConfiguredTargetLabel,
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
