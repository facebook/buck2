/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Instant;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::cycles::CycleGuard;
use buck2_common::dice::file_ops::DiceFileOps;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::error_report::CreateErrorReport;
use buck2_common::file_ops::FileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::dice::LegacyBuckConfigOnDice;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::package_listing::dice::HasPackageListingResolver;
use buck2_common::package_listing::listing::PackageListing;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_common::result::ToUnsharedResultExt;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::name::CellName;
use buck2_core::package::PackageLabel;
use buck2_events::dispatch::span;
use buck2_events::dispatch::span_async;
use buck2_interpreter::dice::starlark_profiler::GetStarlarkProfilerInstrumentation;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::file_loader::ModuleDeps;
use buck2_interpreter::import_paths::HasImportPaths;
use buck2_interpreter::path::OwnedStarlarkModulePath;
use buck2_interpreter::path::PackageFilePath;
use buck2_interpreter::path::StarlarkModulePath;
use buck2_interpreter::path::StarlarkPath;
use buck2_interpreter::starlark_profiler::StarlarkProfilerInstrumentation;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_node::nodes::eval_result::EvaluationResult;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::future;
use starlark::codemap::FileSpan;
use starlark::syntax::AstModule;

use crate::interpreter::cycles::LoadCycleDescriptor;
use crate::interpreter::dice_calculation_delegate::keys::EvalImportKey;
use crate::interpreter::global_interpreter_state::HasGlobalInterpreterState;
use crate::interpreter::interpreter_for_cell::InterpreterForCell;
use crate::interpreter::interpreter_for_cell::ParseResult;
use crate::load_signals::HasLoadSignals;
use crate::super_package::data::SuperPackage;

#[derive(Debug, thiserror::Error)]
enum DiceCalculationDelegateError {
    #[error("Error evaluating build file: `{0}`")]
    EvalBuildFileError(BuildFilePath),
    #[error("Error evaluating module: `{0}`")]
    EvalModuleError(String),
}

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
        let content =
            <dyn FileOps>::read_file(&self.fs, starlark_path.path().as_ref().as_ref()).await?;
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
        let fut = self.eval_deps(&imports);
        let deps = LoadCycleDescriptor::guard_this(self.ctx, fut).await???;
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
            .with_context(|| {
                DiceCalculationDelegateError::EvalModuleError(starlark_file.to_string())
            })?;

        Ok(LoadedModule::new(
            OwnedStarlarkModulePath::new(starlark_file),
            loaded_modules,
            evaluation,
        ))
    }

    /// Eval parent `PACKAGE` file for given `PACKAGE` file.
    async fn eval_parent_package_file(
        &self,
        file: &PackageFilePath,
    ) -> anyhow::Result<SuperPackage> {
        match file.parent_package_file() {
            None => {
                // We are in the cell root, there's no parent.
                Ok(SuperPackage::default())
            }
            Some(parent) => self.eval_package_file(&parent).await,
        }
    }

    /// Return `None` if there's no `PACKAGE` file in the directory.
    pub async fn prepare_package_file_eval(
        &self,
        path: &PackageFilePath,
    ) -> anyhow::Result<Option<(AstModule, ModuleDeps)>> {
        // This is cached if evaluating a `PACKAGE` file next to a `BUCK` file.
        let dir = self.fs.read_dir_with_ignores(path.dir()).await?;
        // Note:
        // * we are using `read_dir` instead of `read_path_metadata` because
        //   * it is an extra IO, and `read_dir` is likely already cached.
        //   * `read_path_metadata` would not tell us if the file name is `PACKAGE`
        //     and not `package` on case-insensitive filesystems.
        //     We do case-sensitive comparison for `BUCK` files, so we do the same here.
        //   * we fail here if `PACKAGE` (but not `package`) exists, and it is not a file.
        if !dir.contains(PackageFilePath::PACKAGE_FILE_NAME) {
            return Ok(None);
        }
        Ok(Some(
            self.prepare_eval(StarlarkPath::PackageFile(path)).await?,
        ))
    }

    async fn eval_package_file_uncached(
        &self,
        path: &PackageFilePath,
    ) -> anyhow::Result<SuperPackage> {
        let parent = self.eval_parent_package_file(path);
        let prepare_eval = self.prepare_package_file_eval(path);

        let (parent, ast_deps) = future::try_join(parent, prepare_eval).await?;

        let (ast, deps) = match ast_deps {
            Some(x) => x,
            None => return Ok(parent),
        };

        let starlark_profiler_instrumentation =
            self.ctx.get_starlark_profiler_instrumentation().await?;

        let buckconfig = self.get_legacy_buck_config_for_starlark().await?;
        let root_buckconfig = self.ctx.get_legacy_root_config_on_dice().await?;
        self.configs
            .eval_package_file(
                path,
                ast,
                parent,
                &buckconfig,
                &root_buckconfig,
                deps.get_loaded_modules(),
                starlark_profiler_instrumentation,
            )
            .with_context(|| format!("evaluating Starlark PACKAGE file `{}`", path))
    }

    async fn eval_package_file(&self, path: &PackageFilePath) -> anyhow::Result<SuperPackage> {
        #[derive(Debug, Display, Clone, Allocative, Eq, PartialEq, Hash)]
        struct PackageFileKey(PackageFilePath);

        #[async_trait]
        impl Key for PackageFileKey {
            type Value = SharedResult<SuperPackage>;

            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let interpreter = ctx
                    .get_interpreter_calculator(self.0.cell(), self.0.build_file_cell())
                    .await
                    .shared_error()?;
                interpreter
                    .eval_package_file_uncached(&self.0)
                    .await
                    .shared_error()
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }

            fn validity(x: &Self::Value) -> bool {
                x.is_ok()
            }
        }

        self.ctx
            .compute(&PackageFileKey(path.clone()))
            .await?
            .unshared_error()
    }

    /// Most directories do not contain a `PACKAGE` file, this function
    /// optimizes `eval_package_file` for this case by avoiding creation of DICE key.
    async fn eval_package_file_for_build_file(
        &self,
        package: PackageLabel,
        package_listing: &PackageListing,
    ) -> anyhow::Result<SuperPackage> {
        let package_file_path = PackageFilePath::for_dir(package.as_cell_path());
        if package_listing
            .get_file(PackageFilePath::PACKAGE_FILE_NAME.as_ref())
            .is_none()
        {
            // Without this optimization, `cquery <that android target>` has 6% time regression.
            // With this optimization, check for `PACKAGE` files adds 2% to time.
            self.eval_parent_package_file(&package_file_path).await
        } else {
            self.eval_package_file(&package_file_path).await
        }
    }

    async fn resolve_package_listing(
        &self,
        package: PackageLabel,
    ) -> anyhow::Result<PackageListing> {
        span_async(
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
        .await
    }

    pub async fn eval_build_file(
        &self,
        package: PackageLabel,
        profiler: &mut StarlarkProfilerOrInstrumentation<'_>,
    ) -> anyhow::Result<EvaluationResult> {
        let now = Instant::now();
        let listing = self.resolve_package_listing(package.dupe()).await?;

        let build_file_path = BuildFilePath::new(package.dupe(), listing.buildfile().to_owned());
        let ast_deps = self.prepare_eval(StarlarkPath::BuildFile(&build_file_path));

        let super_package = self.eval_package_file_for_build_file(package.dupe(), &listing);

        let ((ast, deps), super_package) = future::try_join(ast_deps, super_package).await?;

        let package_boundary_exception = self
            .ctx
            .get_package_boundary_exception(package.as_cell_path())
            .await?;
        let buckconfig = self.get_legacy_buck_config_for_starlark().await?;
        let root_buckconfig = self.ctx.get_legacy_root_config_on_dice().await?;
        let module_id = build_file_path.to_string();
        let cell_str = build_file_path.cell().as_str().to_owned();
        let start_event = buck2_data::LoadBuildFileStart {
            cell: cell_str.clone(),
            module_id: module_id.clone(),
        };
        span(start_event, move || {
            let result = self
                .configs
                .eval_build_file(
                    &build_file_path,
                    &buckconfig,
                    &root_buckconfig,
                    listing,
                    super_package,
                    package_boundary_exception,
                    ast,
                    deps.get_loaded_modules(),
                    profiler,
                )
                .with_context(|| DiceCalculationDelegateError::EvalBuildFileError(build_file_path));
            let error = result.as_ref().err().map(|e| format!("{:#}", e));

            if let Ok(res) = result.as_ref() {
                if let Some(signals) = self.ctx.per_transaction_data().get_load_signals() {
                    signals.send_load(package, res, now.elapsed());
                }
            }

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
