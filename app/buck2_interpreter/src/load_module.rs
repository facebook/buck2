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
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::package::PackageLabel;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use starlark::environment::Globals;

use crate::file_loader::LoadedModule;
use crate::file_loader::ModuleDeps;
use crate::paths::module::StarlarkModulePath;
use crate::paths::package::PackageFilePath;
use crate::prelude_path::PreludePath;

#[async_trait]
pub trait InterpreterCalculationImpl: Send + Sync + 'static {
    async fn get_loaded_module(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: StarlarkModulePath<'_>,
    ) -> buck2_error::Result<LoadedModule>;

    async fn get_module_deps(
        &self,
        ctx: &mut DiceComputations<'_>,
        package: PackageLabel,
        build_file_cell: BuildFileCell,
    ) -> buck2_error::Result<ModuleDeps>;

    /// Return `None` if the PACKAGE file doesn't exist.
    async fn get_package_file_deps(
        &self,
        ctx: &mut DiceComputations<'_>,
        package: PackageLabel,
    ) -> buck2_error::Result<Option<(PackageFilePath, Vec<ImportPath>)>>;

    async fn global_env(&self, ctx: &mut DiceComputations<'_>) -> buck2_error::Result<Globals>;

    async fn prelude_import(
        &self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Option<PreludePath>>;
}

pub static INTERPRETER_CALCULATION_IMPL: LateBinding<&'static dyn InterpreterCalculationImpl> =
    LateBinding::new("INTERPRETER_CALCULATION_IMPL");

#[async_trait]
pub trait InterpreterCalculation {
    /// Returns the LoadedModule for a given starlark file. This is cached on the dice graph.
    async fn get_loaded_module(
        &mut self,
        path: StarlarkModulePath<'_>,
    ) -> buck2_error::Result<LoadedModule>;

    async fn get_loaded_module_from_import_path(
        &mut self,
        path: &ImportPath,
    ) -> buck2_error::Result<LoadedModule> {
        self.get_loaded_module(StarlarkModulePath::LoadFile(path))
            .await
    }

    async fn get_loaded_module_imports(
        &mut self,
        path: &ImportPath,
    ) -> buck2_error::Result<Vec<ImportPath>> {
        //TODO(benfoxman): Don't need to get the whole module, just parse the imports.
        Ok(self
            .get_loaded_module_from_import_path(path)
            .await?
            .imports()
            .cloned()
            .collect())
    }
}

#[async_trait]
impl InterpreterCalculation for DiceComputations<'_> {
    async fn get_loaded_module(
        &mut self,
        path: StarlarkModulePath<'_>,
    ) -> buck2_error::Result<LoadedModule> {
        INTERPRETER_CALCULATION_IMPL
            .get()?
            .get_loaded_module(self, path)
            .await
    }
}
