/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::DiceFileOps;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::dice::LegacyBuckConfigOnDice;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::package_listing::listing::PackageListing;
use buck2_common::package_listing::resolver::PackageListingResolver;
use buck2_common::result::SharedResult;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellName;
use buck2_core::package::Package;
use buck2_events::dispatch::span;
use buck2_events::dispatch::span_async;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use gazebo::prelude::*;
use starlark::codemap::FileSpan;
use starlark::syntax::AstModule;
use thiserror::Error;

use crate::common::OwnedStarlarkModulePath;
use crate::common::StarlarkModulePath;
use crate::common::StarlarkPath;
use crate::dice::calculation::keys::EvalImportKey;
use crate::dice::starlark_profiler::GetStarlarkProfilerInstrumentation;
use crate::dice::starlark_types::GetDisableStarlarkTypes;
use crate::dice::HasCalculationDelegate;
use crate::dice::HasGlobalInterpreterState;
use crate::dice::HasInterpreterContext;
use crate::dice::HasPackageListingResolver;
use crate::extra::ExtraContext;
use crate::file_loader::LoadedModule;
use crate::file_loader::ModuleDeps;
use crate::import_paths::HasImportPaths;
use crate::interpreter::GlobalInterpreterState;
use crate::interpreter::InterpreterConfigForCell;
use crate::interpreter::InterpreterForCell;
use crate::interpreter::ParseResult;
use crate::starlark_profiler::StarlarkProfilerInstrumentation;
use crate::starlark_profiler::StarlarkProfilerOrInstrumentation;

#[derive(Debug, Error)]
enum ConfigError {
    #[error("Couldn't find .buckconfig associated with the cell `{0}`")]
    UnknownCell(CellName),
}

#[derive(Debug, Error)]
#[error("Error evaluating build file: `{0}`")]
pub struct EvalBuildFileError(BuildFilePath);

#[derive(Debug, Error)]
#[error("Error evaluating module: `{0}`")]
pub struct EvalModuleError(String);

#[async_trait]
impl<'c> HasCalculationDelegate<'c> for DiceComputations {
    async fn get_interpreter_calculator(
        &'c self,
        cell: &CellName,
        build_file_cell: &BuildFileCell,
    ) -> anyhow::Result<DiceCalculationDelegate<'c>> {
        #[derive(Clone, Dupe, Allocative)]
        struct ConfigsValue(Arc<HashMap<CellName, Arc<InterpreterConfigForCell>>>);

        #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
        #[display(fmt = "{:?}", self)]
        struct ConfigsKey();

        #[async_trait]
        impl Key for ConfigsKey {
            type Value = SharedResult<ConfigsValue>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let cell_resolver = ctx.get_cell_resolver().await?;
                let global_state = ctx.get_global_interpreter_state().await?;

                let mut configs = HashMap::new();
                for (name, cell) in cell_resolver.cells() {
                    configs.insert(
                        name.clone(),
                        Arc::new(InterpreterConfigForCell::new(
                            cell.cell_alias_resolver().dupe(),
                            global_state.dupe(),
                        )?),
                    );
                }

                Ok(ConfigsValue(Arc::new(configs)))
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                false
            }
        }

        let file_ops = self.file_ops();
        let configs = self.compute(&ConfigsKey()).await??;

        Ok(DiceCalculationDelegate {
            build_file_cell: build_file_cell.clone(),
            ctx: self,
            fs: file_ops,
            configs: configs
                .0
                .get(cell)
                .ok_or_else(|| ConfigError::UnknownCell(cell.clone()))?
                .dupe(),
        })
    }
}

#[async_trait]
impl HasGlobalInterpreterState for DiceComputations {
    async fn get_global_interpreter_state(&self) -> SharedResult<Arc<GlobalInterpreterState>> {
        #[derive(Clone, Dupe, Allocative)]
        struct GisValue(Arc<GlobalInterpreterState>);

        #[derive(Clone, Display, Dupe, Debug, Eq, Hash, PartialEq, Allocative)]
        #[display(fmt = "{:?}", self)]
        struct GisKey();

        #[async_trait]
        impl Key for GisKey {
            type Value = SharedResult<GisValue>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let interpreter_configuror = ctx.get_interpreter_configuror().await?;
                let legacy_configs = ctx.get_legacy_configs_on_dice().await?;
                let cell_resolver = ctx.get_cell_resolver().await?;
                let disable_starlark_types = ctx.get_disable_starlark_types().await?;

                Ok(GisValue(Arc::new(GlobalInterpreterState::new(
                    &legacy_configs,
                    cell_resolver,
                    interpreter_configuror,
                    disable_starlark_types,
                )?)))
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                false
            }
        }

        Ok(self.compute(&GisKey()).await??.0)
    }
}

#[derive(Clone)]
pub struct DiceCalculationDelegate<'c> {
    build_file_cell: BuildFileCell,
    ctx: &'c DiceComputations,
    fs: DiceFileOps<'c>,
    configs: Arc<InterpreterConfigForCell>,
}

impl<'c> DiceCalculationDelegate<'c> {
    /// InterpreterCalculation invokes eval_import recursively. To support
    /// an embedder caching the result of that, the InterpreterCalculation will
    /// call into the delegate instead of calling itself directly.
    ///
    /// The delegate implementation should have roughly the same behavior as
    /// just forwarding directly back to the calculation.
    ///
    /// ```ignore
    /// async fn eval_module(...) -> ... { calculation.eval_module(...).await }
    /// ```
    pub async fn eval_module(
        &self,
        starlark_path: StarlarkModulePath<'_>,
    ) -> SharedResult<LoadedModule> {
        #[async_trait]
        impl Key for EvalImportKey {
            type Value = SharedResult<LoadedModule>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let starlark_path = self.0.borrow();
                let starlark_profiler_instrumentation =
                    ctx.get_starlark_profiler_instrumentation().await?;
                // We cannot just use the inner default delegate's eval_import
                // because that wouldn't delegate back to us for inner eval_import calls.
                Ok(ctx
                    .get_interpreter_calculator(
                        starlark_path.cell(),
                        starlark_path.build_file_cell(),
                    )
                    .await?
                    .eval_module_uncached(starlark_path, starlark_profiler_instrumentation)
                    .await?)
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                // While it is technically possible to compare the modules
                // at least for simple modules (like modules defining only string constants),
                // practically it is too hard to make it work correctly for every case.
                false
            }

            fn validity(x: &Self::Value) -> bool {
                x.is_ok()
            }
        }

        self.ctx
            .compute(&EvalImportKey(OwnedStarlarkModulePath::new(starlark_path)))
            .await?
    }

    fn get_file_ops(&self) -> &dyn FileOps {
        &self.fs
    }

    async fn resolve_package_listing(&self, package: &Package) -> SharedResult<PackageListing> {
        self.ctx
            .get_package_listing_resolver()
            .resolve(package)
            .await
    }

    async fn get_interpreter_for_cell(
        &self,
    ) -> anyhow::Result<crate::interpreter::InterpreterForCell> {
        // NOTE(nga): this takes build file cell, not cell.
        //   Not sure this is correct, but this is how it worked before D34278161.
        let import_paths = self
            .ctx
            .import_paths_for_cell(&self.build_file_cell)
            .await?;
        Ok(InterpreterForCell::new(self.configs.dupe(), import_paths))
    }

    async fn get_legacy_buck_config_for_starlark(
        &self,
    ) -> anyhow::Result<LegacyBuckConfigOnDice<'c>> {
        self.ctx
            .get_legacy_configs_on_dice()
            .await?
            .get(self.build_file_cell.name())
    }

    async fn get_package_boundary_exception(&self, path: &CellPath) -> SharedResult<bool> {
        self.ctx.get_package_boundary_exception(path).await
    }

    async fn parse_file(&self, starlark_path: StarlarkPath<'_>) -> anyhow::Result<ParseResult> {
        let content = self.get_file_ops().read_file(&starlark_path.path()).await?;
        self.parse_file_with_content(starlark_path, content).await
    }

    async fn parse_file_with_content(
        &self,
        starlark_path: StarlarkPath<'_>,
        content: String,
    ) -> anyhow::Result<ParseResult> {
        self.get_interpreter_for_cell()
            .await?
            .parse(starlark_path, content)
    }

    async fn eval_deps(
        &self,
        modules: &[(Option<FileSpan>, OwnedStarlarkModulePath)],
    ) -> anyhow::Result<ModuleDeps> {
        Ok(ModuleDeps(
            futures::future::join_all(modules.iter().map(|(span, import)| async move {
                self.eval_module(import.borrow()).await.with_context(|| {
                    format!(
                        "From `load` at {}",
                        span.as_ref()
                            .map_or("implicit location".to_owned(), FileSpan::to_string)
                    )
                })
            }))
            .await
            .into_iter()
            .collect::<anyhow::Result<_>>()?,
        ))
    }

    pub async fn prepare_eval<'a>(
        &'a self,
        starlark_file: StarlarkPath<'_>,
    ) -> anyhow::Result<(AstModule, ModuleDeps)> {
        let ParseResult(ast, imports) = self.parse_file(starlark_file).await?;
        let deps = self.eval_deps(&imports).await?;
        Ok((ast, deps))
    }

    pub async fn prepare_eval_with_content<'a>(
        &'a self,
        starlark_file: StarlarkPath<'_>,
        content: String,
    ) -> anyhow::Result<AstModule> {
        let ParseResult(ast, _) = self.parse_file_with_content(starlark_file, content).await?;
        Ok(ast)
    }

    pub async fn resolve_load(
        &self,
        starlark_file: StarlarkPath<'_>,
        load_string: &str,
    ) -> anyhow::Result<OwnedStarlarkModulePath> {
        self.get_interpreter_for_cell()
            .await?
            .resolve_path(starlark_file, load_string)
    }

    pub(crate) async fn eval_module_uncached(
        &self,
        starlark_file: StarlarkModulePath<'_>,
        starlark_profiler_instrumentation: Option<StarlarkProfilerInstrumentation>,
    ) -> anyhow::Result<LoadedModule> {
        let (ast, deps) = self.prepare_eval(starlark_file.into()).await?;
        let loaded_modules = deps.get_loaded_modules();
        let buckconfig = self.get_legacy_buck_config_for_starlark().await?;

        let evaluation = self
            .get_interpreter_for_cell()
            .await?
            .eval_module(
                starlark_file,
                &buckconfig,
                ast,
                loaded_modules.clone(),
                starlark_profiler_instrumentation,
            )
            .with_context(|| EvalModuleError(starlark_file.to_string()))?;

        Ok(LoadedModule::new(
            OwnedStarlarkModulePath::new(starlark_file),
            loaded_modules,
            evaluation,
        ))
    }

    pub async fn eval_build_file<T: ExtraContext>(
        &self,
        package: &Package,
        profiler: &mut StarlarkProfilerOrInstrumentation<'_>,
    ) -> anyhow::Result<T::EvalResult> {
        let listing = span_async(
            buck2_data::LoadPackageStart {
                path: package.as_cell_path().to_string(),
            },
            async {
                let result = self.resolve_package_listing(package).await;
                let error = result.as_ref().err().map(|e| format!("{:#}", e));
                (
                    result,
                    buck2_data::LoadPackageEnd {
                        path: package.as_cell_path().to_string(),
                        error,
                    },
                )
            },
        )
        .await?;
        let package_boundary_exception = self
            .get_package_boundary_exception(package.as_cell_path())
            .await?;
        let build_file_path = BuildFilePath::new(package.dupe(), listing.buildfile().to_owned());
        let module_id = build_file_path.id().as_str().to_owned();
        let cell_str = build_file_path.cell().as_str().to_owned();
        let start_event = buck2_data::LoadBuildFileStart {
            cell: cell_str.clone(),
            module_id: module_id.clone(),
        };
        let (ast, deps) = self
            .prepare_eval(StarlarkPath::BuildFile(&build_file_path))
            .await?;
        let interpreter = self.get_interpreter_for_cell().await?;
        let buckconfig = self.get_legacy_buck_config_for_starlark().await?;
        span(start_event, move || {
            let result = interpreter
                .eval_build_file::<T>(
                    &build_file_path,
                    &buckconfig,
                    listing,
                    package_boundary_exception,
                    ast,
                    deps.get_loaded_modules(),
                    profiler,
                )
                .with_context(|| EvalBuildFileError(build_file_path));
            let error = result.as_ref().err().map(|e| format!("{:#}", e));

            (
                result,
                buck2_data::LoadBuildFileEnd {
                    module_id,
                    cell: cell_str,
                    error,
                },
            )
        })
    }
}

mod keys {
    use allocative::Allocative;
    use derive_more::Display;

    use crate::common::OwnedStarlarkModulePath;

    #[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    #[display(fmt = "{}", .0)]
    pub struct EvalImportKey(pub OwnedStarlarkModulePath);
}

pub mod testing {
    // re-exports for testing
    pub use super::keys::EvalImportKey;
}
