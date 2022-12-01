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
use dice::DiceTransaction;
use dice::InjectedKey;
use gazebo::dupe::Dupe;

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
struct DisableStarlarkTypesKey;

impl InjectedKey for DisableStarlarkTypesKey {
    type Value = bool;

    fn compare(x: &bool, y: &bool) -> bool {
        x == y
    }
}

pub trait SetDisableStarlarkTypes {
    fn set_disable_starlark_types(&self, disable_starlark_types: bool) -> anyhow::Result<()>;
}

impl SetDisableStarlarkTypes for DiceTransaction {
    fn set_disable_starlark_types(&self, disable_starlark_types: bool) -> anyhow::Result<()> {
        Ok(self.changed_to([(DisableStarlarkTypesKey, disable_starlark_types)])?)
    }
}

#[async_trait]
pub trait GetDisableStarlarkTypes {
    async fn get_disable_starlark_types(&self) -> anyhow::Result<bool>;
}

#[async_trait]
impl GetDisableStarlarkTypes for DiceComputations {
    async fn get_disable_starlark_types(&self) -> anyhow::Result<bool> {
        Ok(self.compute(&DisableStarlarkTypesKey).await?)
    }
}
