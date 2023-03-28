/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceTransactionUpdater;
use dice::InjectedKey;
use dupe::Dupe;

use crate::interpreter::configuror::BuildInterpreterConfiguror;

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "{:?}", self)]
struct BuildContextKey();

impl InjectedKey for BuildContextKey {
    type Value = Arc<BuildInterpreterConfiguror>;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[async_trait]
pub trait HasInterpreterContext {
    async fn get_interpreter_configuror(&self) -> anyhow::Result<Arc<BuildInterpreterConfiguror>>;
}

#[async_trait]
impl HasInterpreterContext for DiceComputations {
    async fn get_interpreter_configuror(&self) -> anyhow::Result<Arc<BuildInterpreterConfiguror>> {
        Ok(self.compute(&BuildContextKey()).await?.dupe())
    }
}

pub trait SetInterpreterContext {
    fn set_interpreter_context(
        &mut self,
        interpreter_configuror: Arc<BuildInterpreterConfiguror>,
    ) -> anyhow::Result<()>;
}

impl SetInterpreterContext for DiceTransactionUpdater {
    fn set_interpreter_context(
        &mut self,
        interpreter_configuror: Arc<BuildInterpreterConfiguror>,
    ) -> anyhow::Result<()> {
        Ok(self.changed_to(vec![(BuildContextKey(), interpreter_configuror)])?)
    }
}
