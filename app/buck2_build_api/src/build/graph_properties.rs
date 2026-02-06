/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

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

use crate::build::detailed_aggregated_metrics::buck2_sketches::AnalysisGraphPropertiesKey;
use crate::build::detailed_aggregated_metrics::buck2_sketches::compute_configured_graph_sketch;
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
            compute_configured_graph_sketch(configured_node, self.configured_graph_sketch)
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
    compute_configured_graph_sketch(node, configured_graph_sketch)
}
