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
use std::hash::BuildHasherDefault;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use base64::write::EncoderWriter;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_util::commas::commas;
use buck2_util::strong_hasher::Blake3StrongHasher;
use derivative::Derivative;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::FutureExt;
use fxhash::FxHashSet;
use ref_cast::RefCast;
use setsketch::SetSketchParams;
use setsketch::SetSketcher;
use starlark::values::FrozenHeapRef;
use starlark_map::ordered_map::OrderedMap;
use strong_hash::StrongHash;
use strong_hash::UseStrongHashing;

use crate::analysis::calculation::RuleAnalysisCalculation;

#[derive(Copy, Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative, Default)]
pub struct GraphPropertiesOptions {
    pub configured_graph_size: bool,
    pub configured_graph_sketch: bool,
    pub configured_graph_unconfigured_sketch: bool,
    pub total_configured_graph_sketch: bool,
    pub total_configured_graph_unconfigured_sketch: bool,
    pub retained_analysis_memory_sketch: bool,
}

impl fmt::Display for GraphPropertiesOptions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            configured_graph_size,
            configured_graph_sketch,
            configured_graph_unconfigured_sketch,
            total_configured_graph_sketch,
            total_configured_graph_unconfigured_sketch,
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

        if configured_graph_unconfigured_sketch {
            comma(f)?;
            write!(f, "configured_graph_unconfigured_sketch")?;
        }

        if total_configured_graph_sketch {
            comma(f)?;
            write!(f, "total_configured_graph_sketch")?;
        }

        if total_configured_graph_unconfigured_sketch {
            comma(f)?;
            write!(f, "total_configured_graph_unconfigured_sketch")?;
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
            configured_graph_unconfigured_sketch,
            total_configured_graph_sketch,
            total_configured_graph_unconfigured_sketch,
            retained_analysis_memory_sketch,
        } = self;

        !configured_graph_size
            && !configured_graph_sketch
            && !configured_graph_unconfigured_sketch
            && !total_configured_graph_sketch
            && !total_configured_graph_unconfigured_sketch
            && !retained_analysis_memory_sketch
    }

    pub(crate) fn should_compute_configured_graph_sketch(self) -> bool {
        self.configured_graph_sketch || self.total_configured_graph_sketch
    }

    pub(crate) fn should_compute_per_configuration_sketch(self) -> bool {
        self.configured_graph_unconfigured_sketch || self.total_configured_graph_unconfigured_sketch
    }
}

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Allocative)]
pub struct ConfiguredGraphPropertiesValues {
    pub configured_graph_size: u64,
    pub configured_graph_sketch: Option<MergeableGraphSketch<ConfiguredTargetLabel>>,
    pub per_configuration_sketch:
        Option<Arc<OrderedMap<ConfigurationData, MergeableGraphSketch<TargetLabel>>>>,
}

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Allocative)]
pub struct GraphPropertiesValues {
    pub configured: ConfiguredGraphPropertiesValues,
    pub retained_analysis_memory_sketch: Option<MergeableGraphSketch<StarlarkEvalKind>>,
}

/// This is a struct representing graph sketches returned from DICE call to compute sketches.
/// It satisfies 2 properties.
/// (1) It can be merged with other sketches via VersionedSketcher's `merge` method. It does
/// so by holding directly onto the `SetSketcher` type.
/// (2) It implements Dupe, Hash, and Eq. Hash and Eq are implemented by precomputing and holding
/// onto a signature of the sketch.
#[derive(Clone, Dupe, Derivative, Allocative)]
#[derivative(Debug)]
pub struct MergeableGraphSketch<T: StrongHash> {
    version: SketchVersion,
    #[derivative(Debug = "ignore", PartialEq = "ignore", Hash = "ignore")]
    #[allocative(skip)] // TODO(scottcao): Figure out how to implement allocative properly
    sketcher: Arc<SetSketcher<UseStrongHashing<T>, Blake3StrongHasher>>,
}

impl<T: StrongHash> PartialEq for MergeableGraphSketch<T> {
    fn eq(&self, other: &Self) -> bool {
        self.version == other.version
            && self.sketcher.get_registers() == other.sketcher.get_registers()
    }
}

impl<T: StrongHash> Eq for MergeableGraphSketch<T> {}

impl<T: StrongHash> MergeableGraphSketch<T> {
    fn new(
        version: SketchVersion,
        sketcher: SetSketcher<UseStrongHashing<T>, Blake3StrongHasher>,
    ) -> Self {
        let sketcher = Arc::new(sketcher);
        Self { version, sketcher }
    }

    pub fn serialize(&self) -> String {
        let mut res = format!("{}:", self.version).into_bytes();
        let mut enc =
            EncoderWriter::new(&mut res, &base64::engine::general_purpose::STANDARD_NO_PAD);
        for v in self.sketcher.get_registers() {
            use std::io::Write;
            enc.write_all(&v.to_ne_bytes()).unwrap();
        }
        enc.finish().unwrap();
        drop(enc);
        String::from_utf8(res).unwrap()
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
#[display(
    "GraphPropertiesKey: {}, configured_graph_sketch={}, per_configuration_sketch={}",
    label,
    configured_graph_sketch,
    per_configuration_sketch
)]
struct ConfiguredGraphPropertiesKey {
    label: ConfiguredTargetLabel,
    configured_graph_sketch: bool,
    per_configuration_sketch: bool,
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
                self.per_configuration_sketch,
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

#[derive(
    Copy,
    Clone,
    Dupe,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Allocative,
    derive_more::Display
)]
pub(crate) enum SketchVersion {
    V1,
}

pub(crate) static DEFAULT_SKETCH_VERSION: SketchVersion = SketchVersion::V1;

impl SketchVersion {
    pub(crate) fn create_sketcher<T: StrongHash>(self) -> VersionedSketcher<T> {
        let sketcher = match self {
            Self::V1 => SetSketcher::new(
                SetSketchParams::recommended(),
                BuildHasherDefault::<Blake3StrongHasher>::new(), // We want a predictable hash here.
            ),
        };

        VersionedSketcher {
            version: self,
            sketcher,
        }
    }
}

pub(crate) struct VersionedSketcher<T: StrongHash> {
    version: SketchVersion,
    sketcher: SetSketcher<UseStrongHashing<T>, Blake3StrongHasher>,
}

impl<T: StrongHash> VersionedSketcher<T> {
    fn sketch(&mut self, t: &T) {
        self.sketcher.sketch(UseStrongHashing::ref_cast(t));
    }

    fn sketch_weighted(&mut self, t: &T, weight: u64) {
        self.sketcher
            .sketch_weighted_locality_unstable(UseStrongHashing::ref_cast(t), weight);
    }

    pub(crate) fn into_mergeable_graph_sketch(self) -> MergeableGraphSketch<T> {
        MergeableGraphSketch::new(self.version, self.sketcher)
    }

    pub(crate) fn merge(&mut self, other: &MergeableGraphSketch<T>) -> buck2_error::Result<()> {
        if self.version == other.version {
            self.sketcher.merge(&other.sketcher);
            Ok(())
        } else {
            Err(buck2_error::internal_error!(
                // This is currently an internal error because users cannot specify sketch version to use.
                "Set sketch version mismatch between {} and {}. Cannot merge.",
                self.version,
                other.version
            ))
        }
    }
}

pub(crate) struct VersionedSketcherMap<K: Dupe + Hash + Eq, T: StrongHash> {
    map: OrderedMap<K, VersionedSketcher<T>>,
    version: SketchVersion,
}

impl<K: Dupe + Hash + Eq, T: StrongHash> VersionedSketcherMap<K, T> {
    pub(crate) fn new(version: SketchVersion) -> Self {
        Self {
            map: OrderedMap::new(),
            version,
        }
    }

    pub(crate) fn entry_or_insert(&mut self, key: K) -> &mut VersionedSketcher<T> {
        self.map
            .entry(key)
            .or_insert_with(|| self.version.create_sketcher())
    }

    pub(crate) fn into_mergeable_graph_sketch_map(self) -> OrderedMap<K, MergeableGraphSketch<T>> {
        self.map
            .into_iter()
            .map(|(k, v)| (k, v.into_mergeable_graph_sketch()))
            .collect()
    }
}

/// Returns the total graph size for all dependencies of a target.
pub async fn get_graph_properties(
    ctx: &mut DiceComputations<'_>,
    label: &ConfiguredTargetLabel,
    configured_graph_sketch: bool,
    per_configuration_sketch: bool,
    retained_analysis_memory_sketch: bool,
) -> buck2_error::Result<MaybeCompatible<GraphPropertiesValues>> {
    let (conf, analysis) = ctx
        .try_compute2(
            |ctx| {
                async {
                    ctx.compute(&ConfiguredGraphPropertiesKey {
                        label: label.dupe(),
                        configured_graph_sketch,
                        per_configuration_sketch,
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
    per_configuration_sketch: bool,
) -> ConfiguredGraphPropertiesValues {
    let mut queue = vec![&node];
    let mut visited: HashSet<_, fxhash::FxBuildHasher> = HashSet::default();
    visited.insert(&node);

    let mut configured_graph_sketch = if configured_graph_sketch {
        Some(DEFAULT_SKETCH_VERSION.create_sketcher())
    } else {
        None
    };
    let mut per_configuration_sketch: Option<VersionedSketcherMap<ConfigurationData, TargetLabel>> =
        if per_configuration_sketch {
            Some(VersionedSketcherMap::new(DEFAULT_SKETCH_VERSION))
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
        if let Some(per_configuration_sketch) = per_configuration_sketch.as_mut() {
            let sketch = per_configuration_sketch.entry_or_insert(item.label().cfg().dupe());
            // Note this may sketch same unconfigured target label multiple times.
            // This is fine because merge(sketch(A), sketch(B)) = sketch(A+B), and it's probably cheaper memory-wise
            // than keeping a set of unconfigured target labels.
            sketch.sketch(item.label().unconfigured());
        }
    }

    ConfiguredGraphPropertiesValues {
        configured_graph_size: visited.len() as _,
        configured_graph_sketch: configured_graph_sketch
            .map(|sketch| sketch.into_mergeable_graph_sketch()),
        per_configuration_sketch: per_configuration_sketch.map(|per_configuration_sketch| {
            Arc::new(per_configuration_sketch.into_mergeable_graph_sketch_map())
        }),
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
