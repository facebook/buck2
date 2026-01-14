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
use base64::Engine;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_util::commas::commas;
use buck2_util::strong_hasher::Blake3StrongHasher;
use derivative::Derivative;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use ref_cast::RefCast;
use setsketch::SetSketchParams;
use setsketch::SetSketcher;
use starlark_map::ordered_map::OrderedMap;
use strong_hash::StrongHash;
use strong_hash::UseStrongHashing;

#[derive(Copy, Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative, Default)]
pub struct GraphPropertiesOptions {
    pub configured_graph_size: bool,
    pub configured_graph_sketch: bool,
    pub configured_graph_unconfigured_sketch: bool,
    pub total_configured_graph_sketch: bool,
    pub total_configured_graph_unconfigured_sketch: bool,
    pub total_per_configuration_sketch: bool,
}

impl fmt::Display for GraphPropertiesOptions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            configured_graph_size,
            configured_graph_sketch,
            configured_graph_unconfigured_sketch,
            total_configured_graph_sketch,
            total_configured_graph_unconfigured_sketch,
            total_per_configuration_sketch,
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

        if total_per_configuration_sketch {
            comma(f)?;
            write!(f, "total_per_configuration_sketch")?;
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
            total_per_configuration_sketch,
        } = self;

        !configured_graph_size
            && !configured_graph_sketch
            && !configured_graph_unconfigured_sketch
            && !total_configured_graph_sketch
            && !total_configured_graph_unconfigured_sketch
            && !total_per_configuration_sketch
    }

    pub(crate) fn should_compute_configured_graph_sketch(self) -> bool {
        self.configured_graph_sketch || self.total_configured_graph_sketch
    }

    pub(crate) fn should_compute_per_configuration_sketch(self) -> bool {
        self.configured_graph_unconfigured_sketch
            || self.total_configured_graph_unconfigured_sketch
            || self.total_per_configuration_sketch
    }
}

#[derive(Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative)]
pub struct GraphPropertiesValues {
    pub configured_graph_size: u64,
    pub configured_graph_sketch: Option<MergeableGraphSketch<ConfiguredTargetLabel>>,
    pub per_configuration_sketch:
        Option<Arc<OrderedMap<ConfigurationData, MergeableGraphSketch<TargetLabel>>>>,
}

/// This is a struct representing graph sketches returned from DICE call to compute sketches.
/// It satisfies 2 properties.
/// (1) It can be merged with other sketches via VersionedSketcher's `merge` method. It does
/// so by holding directly onto the `SetSketcher` type.
/// (2) It implements Dupe, Hash, and Eq. Hash and Eq are implemented by precomputing and holding
/// onto a signature of the sketch.
#[derive(Clone, Dupe, Derivative, Allocative)]
#[derivative(Debug, PartialEq, Eq, Hash)]
pub struct MergeableGraphSketch<T: StrongHash> {
    version: SketchVersion,
    signature: Arc<Vec<u8>>,
    #[derivative(Debug = "ignore", PartialEq = "ignore", Hash = "ignore")]
    #[allocative(skip)] // TODO(scottcao): Figure out how to implement allocative properly
    sketcher: Arc<SetSketcher<UseStrongHashing<T>, Blake3StrongHasher>>,
}

impl<T: StrongHash> MergeableGraphSketch<T> {
    fn new(
        version: SketchVersion,
        sketcher: SetSketcher<UseStrongHashing<T>, Blake3StrongHasher>,
    ) -> Self {
        let signature = sketcher.get_registers();
        let signature: Vec<_> = signature.iter().flat_map(|v| v.to_ne_bytes()).collect();
        let signature = Arc::new(signature);
        let sketcher = Arc::new(sketcher);
        Self {
            version,
            signature,
            sketcher,
        }
    }

    pub fn serialize(&self) -> String {
        let mut res = format!("{}:", self.version);
        base64::engine::general_purpose::STANDARD_NO_PAD.encode_string(&*self.signature, &mut res);
        res
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
struct GraphPropertiesKey {
    label: ConfiguredTargetLabel,
    configured_graph_sketch: bool,
    per_configuration_sketch: bool,
}

#[async_trait]
impl Key for GraphPropertiesKey {
    type Value = buck2_error::Result<MaybeCompatible<GraphPropertiesValues>>;

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

    pub(crate) fn merge<'a>(
        &mut self,
        other: impl Iterator<Item = (&'a K, &'a MergeableGraphSketch<T>)>,
    ) -> buck2_error::Result<()>
    where
        K: 'a,
        T: 'a,
    {
        for (k, other_sketch) in other {
            let sketcher = self.entry_or_insert(k.dupe());
            sketcher.merge(other_sketch)?;
        }
        Ok(())
    }

    pub(crate) fn into_mergeable_graph_sketch_map(self) -> OrderedMap<K, MergeableGraphSketch<T>> {
        self.map
            .into_iter()
            .map(|(k, v)| (k, v.into_mergeable_graph_sketch()))
            .collect()
    }

    pub(crate) fn into_iter(self) -> impl Iterator<Item = (K, VersionedSketcher<T>)> {
        self.map.into_iter()
    }
}

/// Returns the total graph size for all dependencies of a target.
pub async fn get_configured_graph_properties(
    ctx: &mut DiceComputations<'_>,
    label: &ConfiguredTargetLabel,
    configured_graph_sketch: bool,
    per_configuration_sketch: bool,
) -> buck2_error::Result<MaybeCompatible<GraphPropertiesValues>> {
    ctx.compute(&GraphPropertiesKey {
        label: label.dupe(),
        configured_graph_sketch,
        per_configuration_sketch,
    })
    .await?
}

/// Returns the total graph size for all dependencies of a target without caching the result on the DICE graph.
/// The cost of storing this on DICE is extremely low, so there's almost no reason to use this function (we currently
/// expose it just for performance testing).
pub fn debug_compute_configured_graph_properties_uncached(
    node: ConfiguredTargetNode,
    configured_graph_sketch: bool,
    per_configuration_sketch: bool,
) -> GraphPropertiesValues {
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

    GraphPropertiesValues {
        configured_graph_size: visited.len() as _,
        configured_graph_sketch: configured_graph_sketch
            .map(|sketch| sketch.into_mergeable_graph_sketch()),
        per_configuration_sketch: per_configuration_sketch.map(|per_configuration_sketch| {
            Arc::new(per_configuration_sketch.into_mergeable_graph_sketch_map())
        }),
    }
}
