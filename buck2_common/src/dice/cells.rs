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

#[async_trait]
pub trait HasCellResolver {
    async fn get_cell_resolver(&self) -> anyhow::Result<CellResolver>;

    fn set_cell_resolver(&self, cell_resolver: CellResolver);
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "{:?}", self)]
struct CellResolverKey;

impl InjectedKey for CellResolverKey {
    type Value = CellResolver;

    fn compare(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[async_trait]
impl HasCellResolver for DiceComputations {
    async fn get_cell_resolver(&self) -> anyhow::Result<CellResolver> {
        Ok(self.compute(&CellResolverKey).await?)
    }

    fn set_cell_resolver(&self, cell_resolver: CellResolver) {
        self.changed_to(vec![(CellResolverKey, cell_resolver)])
    }
}
