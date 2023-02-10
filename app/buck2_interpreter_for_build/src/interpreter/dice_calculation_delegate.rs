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
use anyhow::Context;
use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::DiceFileOps;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::error_report::CreateErrorReport;
use buck2_common::file_ops::FileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::dice::LegacyBuckConfigOnDice;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::package_listing::dice::HasPackageListingResolver;
use buck2_common::result::SharedResult;
use buck2_common::result::ToUnsharedResultExt;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::name::CellName;
use buck2_core::package::PackageLabel;
use buck2_events::dispatch::span;
use buck2_events::dispatch::span_async;
use buck2_interpreter::dice::starlark_profiler::GetStarlarkProfilerInstrumentation;
use buck2_interpreter::extra::ExtraContext;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::file_loader::ModuleDeps;
use buck2_interpreter::import_paths::HasImportPaths;
use buck2_interpreter::path::OwnedStarlarkModulePath;
use buck2_interpreter::path::StarlarkModulePath;
use buck2_interpreter::path::StarlarkPath;
use buck2_interpreter::starlark_profiler::StarlarkProfilerInstrumentation;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use starlark::codemap::FileSpan;
use starlark::syntax::AstModule;
use thiserror::Error;

use crate::interpreter::dice_calculation_delegate::keys::EvalImportKey;
use crate::interpreter::global_interpreter_state::HasGlobalInterpreterState;
use crate::interpreter::interpreter_for_cell::InterpreterForCell;
use crate::interpreter::interpreter_for_cell::ParseResult;

#[derive(Debug, Error)]
#[error("Error evaluating build file: `{0}`")]
pub struct EvalBuildFileError(BuildFilePath);

#[derive(Debug, Error)]
#[error("Error evaluating module: `{0}`")]
pub struct EvalModuleError(String);

#[async_trait]
pub trait HasCalculationDelegate<'c> {
    /// Get calculator for a file evaluation.
    ///
    /// This function only accepts cell names, but it is created
    /// per evaluated file (build file or `.bzl`).
    async fn get_interpreter_calculator(
        &'c self,
        cell: CellName,
        build_file_cell: BuildFileCell,
    ) -> anyhow::Result<DiceCalculationDelegate<'c>>;
}

#[async_trait]
impl<'c> HasCalculationDelegate<'c> for DiceComputations {
    async fn get_interpreter_calculator(
        &'c self,
        cell: CellName,
        build_file_cell: BuildFileCell,
    ) -> anyhow::Result<DiceCalculationDelegate<'c>> {
        #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
        #[display(fmt = "{}@{}", _0, _1)]
        struct InterpreterConfigForCellKey(CellName, BuildFileCell);

        #[async_trait]
        impl Key for InterpreterConfigForCellKey {
            type Value = SharedResult<Arc<InterpreterForCell>>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let cell_resolver = ctx.get_cell_resolver().await?;
                let global_state = ctx.get_global_interpreter_state().await?;

                let cell = cell_resolver.get(self.0)?;

                let implicit_import_paths = ctx.import_paths_for_cell(self.1).await?;

                Ok(Arc::new(InterpreterForCell::new(
                    cell.cell_alias_resolver().dupe(),
                    global_state.dupe(),
                    implicit_import_paths,
                )?))
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                false
            }
        }

        let file_ops = self.file_ops();
        let configs = self
            .compute(&InterpreterConfigForCellKey(cell, build_file_cell))
            .await??;

        Ok(DiceCalculationDelegate {
            build_file_cell,
            ctx: self,
            fs: file_ops,
            configs,
        })
    }
}

#[derive(Clone)]
pub struct DiceCalculationDelegate<'c> {
    build_file_cell: BuildFileCell,
    ctx: &'c DiceComputations,
    fs: DiceFileOps<'c>,
    configs: Arc<InterpreterForCell>,
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
    ) -> anyhow::Result<LoadedModule> {
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
            .unshared_error()
    }

    async fn get_legacy_buck_config_for_starlark(
        &self,
    ) -> anyhow::Result<LegacyBuckConfigOnDice<'c>> {
        self.ctx
            .get_legacy_config_on_dice(self.build_file_cell.name())
            .await
    }

    async fn parse_file(&self, starlark_path: StarlarkPath<'_>) -> anyhow::Result<ParseResult> {
        let content = self
            .fs
            .read_file(starlark_path.path().as_ref().as_ref())
            .await?;
        self.configs.parse(starlark_path, content)
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

    pub fn prepare_eval_with_content<'a>(
        &'a self,
        starlark_file: StarlarkPath<'_>,
        content: String,
    ) -> anyhow::Result<AstModule> {
        let ParseResult(ast, _) = self.configs.parse(starlark_file, content)?;
        Ok(ast)
    }

    pub async fn resolve_load(
        &self,
        starlark_file: StarlarkPath<'_>,
        load_string: &str,
    ) -> anyhow::Result<OwnedStarlarkModulePath> {
        self.configs.resolve_path(starlark_file, load_string)
    }

    pub async fn eval_module_uncached(
        &self,
        starlark_file: StarlarkModulePath<'_>,
        starlark_profiler_instrumentation: Option<StarlarkProfilerInstrumentation>,
    ) -> anyhow::Result<LoadedModule> {
        let (ast, deps) = self.prepare_eval(starlark_file.into()).await?;
        let loaded_modules = deps.get_loaded_modules();
        let buckconfig = self.get_legacy_buck_config_for_starlark().await?;
        let root_buckconfig = self.ctx.get_legacy_root_config_on_dice().await?;

        let evaluation = self
            .configs
            .eval_module(
                starlark_file,
                &buckconfig,
                &root_buckconfig,
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
        package: PackageLabel,
        profiler: &mut StarlarkProfilerOrInstrumentation<'_>,
    ) -> anyhow::Result<T::EvalResult> {
        let listing = span_async(
            buck2_data::LoadPackageStart {
                path: package.as_cell_path().to_string(),
            },
            async {
                let result = self.ctx.resolve_package_listing(package.dupe()).await;
                let error = result.create_error_report();
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
            .ctx
            .get_package_boundary_exception(package.as_cell_path())
            .await?;
        let build_file_path = BuildFilePath::new(package.dupe(), listing.buildfile().to_owned());
        let module_id = build_file_path.to_string();
        let cell_str = build_file_path.cell().as_str().to_owned();
        let start_event = buck2_data::LoadBuildFileStart {
            cell: cell_str.clone(),
            module_id: module_id.clone(),
        };
        let (ast, deps) = self
            .prepare_eval(StarlarkPath::BuildFile(&build_file_path))
            .await?;
        let buckconfig = self.get_legacy_buck_config_for_starlark().await?;
        let root_buckconfig = self.ctx.get_legacy_root_config_on_dice().await?;
        span(start_event, move || {
            let result = self
                .configs
                .eval_build_file::<T>(
                    &build_file_path,
                    &buckconfig,
                    &root_buckconfig,
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
    use buck2_interpreter::path::OwnedStarlarkModulePath;
    use derive_more::Display;

    #[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    pub struct EvalImportKey(pub OwnedStarlarkModulePath);
}

pub mod testing {
    // re-exports for testing
    pub use super::keys::EvalImportKey;
}
