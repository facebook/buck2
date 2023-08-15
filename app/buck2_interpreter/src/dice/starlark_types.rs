/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use async_trait::async_trait;
use dice::DiceComputations;
use dice::DiceTransactionUpdater;
use dice::InjectedKey;
use dupe::Dupe;

#[derive(Debug, Clone, Dupe, Eq, PartialEq, Allocative)]
struct StarlarkTypesValue {
    disable_starlark_types: bool,
    unstable_typecheck: bool,
}

#[derive(
    Debug,
    derive_more::Display,
    Copy,
    Clone,
    Dupe,
    Eq,
    PartialEq,
    Hash,
    Allocative
)]
#[display(fmt = "{:?}", self)]
struct StarlarkTypesKey;

impl InjectedKey for StarlarkTypesKey {
    type Value = StarlarkTypesValue;

    fn equality(x: &StarlarkTypesValue, y: &StarlarkTypesValue) -> bool {
        x == y
    }
}

pub trait SetStarlarkTypes {
    fn set_starlark_types(
        &mut self,
        disable_starlark_types: bool,
        unstable_typecheck: bool,
    ) -> anyhow::Result<()>;
}

impl SetStarlarkTypes for DiceTransactionUpdater {
    fn set_starlark_types(
        &mut self,
        disable_starlark_types: bool,
        unstable_typecheck: bool,
    ) -> anyhow::Result<()> {
        Ok(self.changed_to([(
            StarlarkTypesKey,
            StarlarkTypesValue {
                disable_starlark_types,
                unstable_typecheck,
            },
        )])?)
    }
}

#[async_trait]
pub trait GetStarlarkTypes {
    async fn get_disable_starlark_types(&self) -> anyhow::Result<bool>;
    async fn get_unstable_typecheck(&self) -> anyhow::Result<bool>;
}

#[async_trait]
impl GetStarlarkTypes for DiceComputations {
    async fn get_disable_starlark_types(&self) -> anyhow::Result<bool> {
        Ok(self
            .compute(&StarlarkTypesKey)
            .await?
            .disable_starlark_types)
    }

    async fn get_unstable_typecheck(&self) -> anyhow::Result<bool> {
        Ok(self.compute(&StarlarkTypesKey).await?.unstable_typecheck)
    }
}
