/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::bzl::ImportPath;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

use crate::file_loader::LoadedModule;
use crate::path::StarlarkModulePath;

#[async_trait]
pub trait InterpreterCalculationImpl: Send + Sync + 'static {
    async fn get_loaded_module(
        &self,
        ctx: &DiceComputations,
        path: StarlarkModulePath<'_>,
    ) -> anyhow::Result<LoadedModule>;
}

pub static INTERPRETER_CALCULATION_IMPL: LateBinding<&'static dyn InterpreterCalculationImpl> =
    LateBinding::new("INTERPRETER_CALCULATION_IMPL");

#[async_trait]
pub trait InterpreterCalculation {
    /// Returns the LoadedModule for a given starlark file. This is cached on the dice graph.
    async fn get_loaded_module(&self, path: StarlarkModulePath<'_>)
    -> anyhow::Result<LoadedModule>;

    async fn get_loaded_module_from_import_path(
        &self,
        path: &ImportPath,
    ) -> anyhow::Result<LoadedModule> {
        self.get_loaded_module(StarlarkModulePath::LoadFile(path))
            .await
    }
}

#[async_trait]
impl InterpreterCalculation for DiceComputations {
    async fn get_loaded_module(
        &self,
        path: StarlarkModulePath<'_>,
    ) -> anyhow::Result<LoadedModule> {
        INTERPRETER_CALCULATION_IMPL
            .get()?
            .get_loaded_module(self, path)
            .await
    }
}
