/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Core dice computations relating to cells

use async_trait::async_trait;
use buck2_core::cells::CellResolver;
use derive_more::Display;
use dice::DiceComputations;
use dice::InjectedKey;
use gazebo::prelude::*;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CellResolverKeyError {
    #[error("No cell resolver key was injected into the DICE cache")]
    NoCellResolverKeyFound,
}

#[async_trait]
pub trait HasCellResolver {
    async fn get_cell_resolver(&self) -> anyhow::Result<CellResolver>;

    async fn try_get_cell_resolver(&self) -> anyhow::Result<Option<CellResolver>>;

    fn set_cell_resolver(&self, cell_resolver: CellResolver) -> anyhow::Result<()>;

    fn set_none_cell_resolver(&self) -> anyhow::Result<()>;
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
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
        self.compute(&CellResolverKey)
            .await?
            .ok_or_else(|| CellResolverKeyError::NoCellResolverKeyFound.into())
    }

    async fn try_get_cell_resolver(&self) -> anyhow::Result<Option<CellResolver>> {
        Ok(self.compute(&CellResolverKey).await?)
    }

    fn set_cell_resolver(&self, cell_resolver: CellResolver) -> anyhow::Result<()> {
        Ok(self.changed_to(vec![(CellResolverKey, Some(cell_resolver))])?)
    }

    fn set_none_cell_resolver(&self) -> anyhow::Result<()> {
        Ok(self.changed_to(vec![(CellResolverKey, None)])?)
    }
}
