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
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_sketches::DependencyGraphSketch;
use buck2_sketches::MemoryUsageSketch;
use buck2_util::commas::commas;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dice::ValueSerialize;
use dupe::Dupe;
use futures::FutureExt;
use pagable::Pagable;
use pagable::PagablePanic;
use pagable::PagableSerialize;
use pagable::pagable_typetag;

// TODO: this should live next to `ResultMaybeCompatible` in `buck2_core`, but that
// would require a dependency from `buck2_core` to `dice`.
// We should consider moving ValueSerialize (or a similar trait) to pagable, not dice, to avoid this.
struct ResultMaybeCompatibleValueSerialize<T>(std::marker::PhantomData<T>);

impl<T> ResultMaybeCompatibleValueSerialize<T> {
    fn new() -> Self {
        Self(std::marker::PhantomData)
    }
}

impl<T: Pagable + Allocative + Dupe + Send + Sync + 'static> ValueSerialize
    for ResultMaybeCompatibleValueSerialize<T>
{
    type Value = ResultMaybeCompatible<T>;

    fn pagable_serialize_value(
        &self,
        v: &Self::Value,
        ser: &mut dyn pagable::PagableSerializer,
    ) -> Option<pagable::Result<()>> {
        match v {
            ResultMaybeCompatible::Err(_) => None,
            v => Some(v.pagable_serialize(ser)),
        }
    }

    fn pagable_deserialize_value<'de, D: pagable::PagableDeserializer<'de> + ?Sized>(
        &self,
        deser: &mut D,
    ) -> pagable::Result<Self::Value> {
        pagable::PagableDeserialize::pagable_deserialize(deser)
    }
}

use crate::build::detailed_aggregated_metrics::buck2_sketches::AnalysisGraphPropertiesKey;
use crate::build::detailed_aggregated_metrics::buck2_sketches::compute_configured_graph_sketch;
use crate::build::sketch_impl::MergeableGraphSketch;

#[derive(Copy, Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative, Default)]
pub struct GraphPropertiesOptions {
    pub configured_graph_size: bool,
    pub configured_graph_sketch: bool,
    pub total_configured_graph_sketch: bool,
    pub retained_analysis_memory_sketch: bool,
    pub peak_analysis_memory_sketch: bool,
    pub action_graph_sketch: bool,
}

impl fmt::Display for GraphPropertiesOptions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            configured_graph_size,
            configured_graph_sketch,
            total_configured_graph_sketch,
            retained_analysis_memory_sketch,
            peak_analysis_memory_sketch,
            action_graph_sketch,
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

        if peak_analysis_memory_sketch {
            comma(f)?;
            write!(f, "peak_analysis_memory_sketch")?;
        }

        if action_graph_sketch {
            comma(f)?;
            write!(f, "action_graph_sketch")?;
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
            peak_analysis_memory_sketch,
            action_graph_sketch,
        } = self;

        !configured_graph_size
            && !configured_graph_sketch
            && !total_configured_graph_sketch
            && !retained_analysis_memory_sketch
            && !peak_analysis_memory_sketch
            && !action_graph_sketch
    }

    pub(crate) fn should_compute_configured_graph_sketch(self) -> bool {
        self.configured_graph_sketch || self.total_configured_graph_sketch
    }
}

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Allocative, PagablePanic)]
pub struct ConfiguredGraphPropertiesValues {
    pub configured_graph_size: u64,
    pub configured_graph_sketch:
        Option<MergeableGraphSketch<ConfiguredTargetLabel, DependencyGraphSketch>>,
}

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Allocative)]
pub struct GraphPropertiesValues {
    pub configured: ConfiguredGraphPropertiesValues,
    pub retained_analysis_memory_sketch:
        Option<MergeableGraphSketch<StarlarkEvalKind, MemoryUsageSketch>>,
    pub peak_analysis_memory_sketch:
        Option<MergeableGraphSketch<StarlarkEvalKind, MemoryUsageSketch>>,
}

#[derive(
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Allocative,
    Pagable
)]
#[display(
    "GraphPropertiesKey: {}, configured_graph_sketch={}",
    label,
    configured_graph_sketch
)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct ConfiguredGraphPropertiesKey {
    label: ConfiguredTargetLabel,
    configured_graph_sketch: bool,
}

#[async_trait]
impl Key for ConfiguredGraphPropertiesKey {
    type Value = ResultMaybeCompatible<ConfiguredGraphPropertiesValues>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let configured_node = ctx.get_configured_target_node(&self.label).await?;
        ResultMaybeCompatible::Compatible(compute_configured_graph_sketch(
            configured_node,
            self.configured_graph_sketch,
        ))
    }

    fn equality(a: &Self::Value, b: &Self::Value) -> bool {
        match (a, b) {
            (ResultMaybeCompatible::Compatible(a), ResultMaybeCompatible::Compatible(b)) => a == b,
            (ResultMaybeCompatible::Incompatible(a), ResultMaybeCompatible::Incompatible(b)) => {
                a == b
            }
            _ => false,
        }
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        ResultMaybeCompatibleValueSerialize::<ConfiguredGraphPropertiesValues>::new()
    }
}

/// Returns the total graph size for all dependencies of a target.
pub async fn get_graph_properties(
    ctx: &mut DiceComputations<'_>,
    label: &ConfiguredTargetLabel,
    configured_graph_sketch: bool,
    retained_analysis_memory_sketch: bool,
    peak_analysis_memory_sketch: bool,
) -> ResultMaybeCompatible<GraphPropertiesValues> {
    let (conf, sketches) = ctx
        .compute2(
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
                    let (retained, analysis_peak) =
                        if retained_analysis_memory_sketch || peak_analysis_memory_sketch {
                            ctx.compute(&AnalysisGraphPropertiesKey {
                                label: label.dupe(),
                                compute_retained: retained_analysis_memory_sketch,
                                compute_peak: peak_analysis_memory_sketch,
                            })
                            .await??
                            .to_result_maybe_compatible()?
                        } else {
                            (None, None)
                        };
                    ResultMaybeCompatible::Compatible((retained, analysis_peak))
                }
                .boxed()
            },
        )
        .await;
    let (retained, analysis_peak) = sketches?;
    ResultMaybeCompatible::Compatible(GraphPropertiesValues {
        configured: conf?,
        retained_analysis_memory_sketch: retained,
        peak_analysis_memory_sketch: analysis_peak,
    })
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
