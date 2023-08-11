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
use buck2_core::metadata_key::MetadataKey;
use buck2_core::package::PackageLabel;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use starlark::collections::SmallMap;
use starlark::environment::Globals;

use crate::file_loader::LoadedModule;
use crate::file_loader::ModuleDeps;
use crate::file_type::StarlarkFileType;
use crate::paths::module::StarlarkModulePath;
use crate::paths::package::PackageFilePath;
use crate::prelude_path::PreludePath;

#[async_trait]
pub trait InterpreterCalculationImpl: Send + Sync + 'static {
    async fn get_loaded_module(
        &self,
        ctx: &DiceComputations,
        path: StarlarkModulePath<'_>,
    ) -> anyhow::Result<LoadedModule>;

    async fn get_module_deps(
        &self,
        ctx: &DiceComputations,
        package: PackageLabel,
        build_file_cell: BuildFileCell,
    ) -> anyhow::Result<ModuleDeps>;

    /// Return `None` if the PACKAGE file doesn't exist.
    async fn get_package_file_deps(
        &self,
        ctx: &DiceComputations,
        package: &PackageFilePath,
    ) -> anyhow::Result<Option<Vec<ImportPath>>>;

    async fn global_env_for_file_type(
        &self,
        ctx: &DiceComputations,
        file_type: StarlarkFileType,
    ) -> anyhow::Result<Globals>;

    async fn prelude_import(&self, ctx: &DiceComputations) -> anyhow::Result<Option<PreludePath>>;

    async fn package_values(
        &self,
        ctx: &DiceComputations,
        package: PackageLabel,
    ) -> anyhow::Result<SmallMap<MetadataKey, serde_json::Value>>;
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
