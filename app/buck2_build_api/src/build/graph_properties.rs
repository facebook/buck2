/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::fmt;
use std::hash::BuildHasherDefault;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use base64::Engine;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_error::conversion::from_any_with_tag;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_util::commas::commas;
use buck2_util::strong_hasher::Blake3StrongHasher;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use probminhash::setsketcher::SetSketchParams;
use probminhash::setsketcher::SetSketcher;
use ref_cast::RefCast;
use strong_hash::UseStrongHashing;

#[derive(Copy, Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative, Default)]
pub struct GraphPropertiesOptions {
    pub configured_graph_size: bool,
    pub configured_graph_sketch: bool,
}

impl fmt::Display for GraphPropertiesOptions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            configured_graph_size,
            configured_graph_sketch,
        } = *self;

        let mut comma = commas();

        if configured_graph_size {
            comma(f)?;
            write!(f, "configured_graph_size")?;
        }

        if configured_graph_sketch {
            comma(f)?;
            write!(f, "configured_graph_size")?;
        }

        Ok(())
    }
}

impl GraphPropertiesOptions {
    pub fn is_empty(self) -> bool {
        let Self {
            configured_graph_size,
            configured_graph_sketch,
        } = self;

        !configured_graph_size && !configured_graph_sketch
    }
}

#[derive(Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative)]
pub struct GraphPropertiesValues {
    pub configured_graph_size: Option<u64>,
    pub configured_graph_sketch: Option<ConfiguredGraphSketch>,
}

#[derive(Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative)]
pub struct ConfiguredGraphSketch {
    version: SketchVersion,
    signature: Arc<Vec<u8>>,
}

impl ConfiguredGraphSketch {
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
#[display("GraphPropertiesKey: {} {}", label, properties)]
struct GraphPropertiesKey {
    label: ConfiguredTargetLabel,
    properties: GraphPropertiesOptions,
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
        debug_compute_configured_graph_properties_uncached(configured_node, self.properties)
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
enum SketchVersion {
    V1,
}

impl SketchVersion {
    fn create_sketcher(self) -> VersionedSketcher {
        let sketcher = match self {
            Self::V1 => SetSketcher::<u16, _, _>::new(
                // TODO (stansw): Are these params right?
                SetSketchParams::default(),
                BuildHasherDefault::<Blake3StrongHasher>::new(), // We want a predictable hash here.
            ),
        };

        VersionedSketcher {
            version: self,
            sketcher,
        }
    }
}

struct VersionedSketcher {
    version: SketchVersion,
    sketcher: SetSketcher<u16, UseStrongHashing<ConfiguredTargetLabel>, Blake3StrongHasher>,
}

/// Returns the total graph size for all dependencies of a target.
pub async fn get_configured_graph_properties(
    ctx: &mut DiceComputations<'_>,
    label: &ConfiguredTargetLabel,
    properties: GraphPropertiesOptions,
) -> buck2_error::Result<MaybeCompatible<GraphPropertiesValues>> {
    ctx.compute(&GraphPropertiesKey {
        label: label.dupe(),
        properties,
    })
    .await?
}

/// Returns the total graph size for all dependencies of a target without caching the result on the DICE graph.
/// The cost of storing this on DICE is extremely low, so there's almost no reason to use this function (we currently
/// expose it just for performance testing).
pub fn debug_compute_configured_graph_properties_uncached(
    node: MaybeCompatible<ConfiguredTargetNode>,
    properties: GraphPropertiesOptions,
) -> buck2_error::Result<MaybeCompatible<GraphPropertiesValues>> {
    node.try_map(|node| {
        let mut queue = vec![&node];
        let mut visited: HashSet<_, fxhash::FxBuildHasher> = HashSet::default();
        visited.insert(&node);

        let mut sketch = if properties.configured_graph_sketch {
            Some(SketchVersion::V1.create_sketcher())
        } else {
            None
        };

        while let Some(item) = queue.pop() {
            for d in item.deps() {
                if visited.insert(d) {
                    queue.push(d);
                }
            }

            if let Some(sketch) = sketch.as_mut() {
                let label = UseStrongHashing::ref_cast(item.label());
                sketch
                    .sketcher
                    .sketch(label)
                    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::BuildSketchError))?;
            }
        }

        Ok(GraphPropertiesValues {
            configured_graph_size: Some(visited.len() as _),
            configured_graph_sketch: sketch.map(|sketch| {
                let signature = sketch.sketcher.get_signature();
                let signature = signature.iter().flat_map(|v| v.to_ne_bytes()).collect();
                ConfiguredGraphSketch {
                    version: sketch.version,
                    signature: Arc::new(signature),
                }
            }),
        })
    })
}
