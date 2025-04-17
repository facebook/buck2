/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;

#[derive(
    Copy,
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Allocative,
    Default
)]
pub struct GraphPropertiesOptions {
    pub configured_graph_size: bool,
}

impl GraphPropertiesOptions {
    pub fn is_empty(self) -> bool {
        let Self {
            configured_graph_size,
        } = self;

        !configured_graph_size
    }
}

#[derive(Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative, Default)]
pub struct GraphPropertiesValues {
    pub configured_graph_size: Option<u64>,
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

        Ok(configured_node.map(|node| {
            let mut queue = vec![&node];
            let mut visited = HashSet::new();

            while let Some(item) = queue.pop() {
                if !visited.insert(item) {
                    continue;
                }
                queue.extend(item.deps());
            }

            GraphPropertiesValues {
                configured_graph_size: Some(visited.len() as _),
            }
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
