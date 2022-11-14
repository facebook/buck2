/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Core dice computations relating to cells

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::CellResolver;
use derive_more::Display;
use dice::DiceComputations;
use dice::InjectedKey;
use gazebo::prelude::*;

#[async_trait]
pub trait HasCellResolver {
    async fn get_cell_resolver(&self) -> anyhow::Result<CellResolver>;

    async fn is_cell_resolver_key_set(&self) -> anyhow::Result<bool>;

    fn set_cell_resolver(&self, cell_resolver: CellResolver) -> anyhow::Result<()>;

    fn set_none_cell_resolver(&self) -> anyhow::Result<()>;
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "{:?}", self)]
struct CellResolverKey;

impl InjectedKey for CellResolverKey {
    type Value = Option<CellResolver>;

    fn compare(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Some(x), Some(y)) => x == y,
            (None, None) => true,
            (_, _) => false,
        }
    }
}

#[async_trait]
impl HasCellResolver for DiceComputations {
    async fn get_cell_resolver(&self) -> anyhow::Result<CellResolver> {
        self.compute(&CellResolverKey).await?.ok_or_else(|| {
            panic!("Tried to retrieve CellResolverKey from the graph, but key has None value")
        })
    }

    async fn is_cell_resolver_key_set(&self) -> anyhow::Result<bool> {
        Ok(self.compute(&CellResolverKey).await?.is_some())
    }

    fn set_cell_resolver(&self, cell_resolver: CellResolver) -> anyhow::Result<()> {
        Ok(self.changed_to(vec![(CellResolverKey, Some(cell_resolver))])?)
    }

    fn set_none_cell_resolver(&self) -> anyhow::Result<()> {
        Ok(self.changed_to(vec![(CellResolverKey, None)])?)
    }
}
