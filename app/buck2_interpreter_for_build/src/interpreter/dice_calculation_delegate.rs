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
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::cycles::CycleGuard;
use buck2_common::dice::file_ops::DiceFileOps;
use buck2_common::file_ops::FileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::dice::LegacyBuckConfigOnDice;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::package_listing::dice::DicePackageListingResolver;
use buck2_common::package_listing::listing::PackageListing;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::name::CellName;
use buck2_core::package::PackageLabel;
use buck2_error::Context;
use buck2_events::dispatch::span;
use buck2_events::dispatch::span_async;
use buck2_futures::cancellation::CancellationContext;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::error::BuckStarlarkError;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::file_loader::ModuleDeps;
use buck2_interpreter::import_paths::HasImportPaths;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::paths::module::OwnedStarlarkModulePath;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_interpreter::paths::path::StarlarkPath;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::super_package::SuperPackage;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::future;
use futures::FutureExt;
use indoc::indoc;
use starlark::codemap::FileSpan;
use starlark::environment::Globals;
use starlark::environment::Module;
use starlark::syntax::AstModule;
use starlark::syntax::Dialect;

use crate::interpreter::cycles::LoadCycleDescriptor;
use crate::interpreter::global_interpreter_state::HasGlobalInterpreterState;
use crate::interpreter::interpreter_for_cell::InterpreterForCell;
use crate::interpreter::interpreter_for_cell::ParseData;
use crate::interpreter::interpreter_for_cell::ParseResult;
use crate::super_package::package_value::SuperPackageValuesImpl;

#[derive(Debug, buck2_error::Error)]
enum DiceCalculationDelegateError {
    #[error("Error evaluating build file: `{0}`")]
    EvalBuildFileError(BuildFilePath),
    #[error("Error evaluating module: `{0}`")]
    EvalModuleError(String),
    #[error("Error checking starlark stack size")]
    CheckStarlarkStackSizeError,
}

#[async_trait]
pub trait HasCalculationDelegate<'c, 'd> {
    /// Get calculator for a file evaluation.
    ///
    /// This function only accepts cell names, but it is created
    /// per evaluated file (build file or `.bzl`).
    async fn get_interpreter_calculator(
        &'c mut self,
        cell: CellName,
        build_file_cell: BuildFileCell,
    ) -> anyhow::Result<DiceCalculationDelegate<'c, 'd>>;
}

#[async_trait]
impl<'c, 'd> HasCalculationDelegate<'c, 'd> for DiceComputations<'d> {
    async fn get_interpreter_calculator(
        &'c mut self,
        cell: CellName,
        build_file_cell: BuildFileCell,
    ) -> anyhow::Result<DiceCalculationDelegate<'c, 'd>> {
        #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
        #[display(fmt = "{}@{}", _0, _1)]
        struct InterpreterConfigForCellKey(CellName, BuildFileCell);

        #[async_trait]
        impl Key for InterpreterConfigForCellKey {
            type Value = buck2_error::Result<Arc<InterpreterForCell>>;
            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
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

        let configs = self
            .bad_dice(/* configs */)
            .compute(&InterpreterConfigForCellKey(cell, build_file_cell))
            .await??;

        Ok(DiceCalculationDelegate {
            build_file_cell,
            ctx: self,
            configs,
        })
    }
}

pub struct DiceCalculationDelegate<'c, 'd> {
    build_file_cell: BuildFileCell,
    ctx: &'c mut DiceComputations<'d>,
    configs: Arc<InterpreterForCell>,
}

impl<'c, 'd: 'c> DiceCalculationDelegate<'c, 'd> {
    async fn get_legacy_buck_config_for_starlark(
        &'c self,
    ) -> anyhow::Result<LegacyBuckConfigOnDice<'c, 'd>> {
        self.ctx
            .get_legacy_config_on_dice(self.build_file_cell.name())
            .await
    }

    async fn parse_file(&self, starlark_path: StarlarkPath<'_>) -> anyhow::Result<ParseResult> {
        let content = <dyn FileOps>::read_file(
            &DiceFileOps(self.ctx),
            starlark_path.path().as_ref().as_ref(),
        )
        .await?;
        self.configs.parse(starlark_path, content)
    }

    async fn eval_deps(
        ctx: &mut DiceComputations<'_>,
        modules: &[(Option<FileSpan>, OwnedStarlarkModulePath)],
    ) -> anyhow::Result<ModuleDeps> {
        Ok(ModuleDeps(
            ctx.try_compute_join(modules, |ctx, (span, import)| {
                async move {
                    ctx.bad_dice()
                        .get_loaded_module(import.borrow())
                        .await
                        .with_context(|| {
                            format!(
                                "From load at {}",
                                span.as_ref()
                                    .map_or("implicit location".to_owned(), |file_span| file_span
                                        .resolve()
                                        .begin_file_line()
                                        .to_string())
                            )
                        })
                }
                .boxed()
            })
            .await?,
        ))
    }

    pub async fn prepare_eval<'a>(
        &'a self,
        starlark_file: StarlarkPath<'_>,
    ) -> anyhow::Result<(AstModule, ModuleDeps)> {
        let ParseData(ast, imports) = self.parse_file(starlark_file).await??;
        let mut bad_dice = self.ctx.bad_dice(/* cycles */);
        let fut = Self::eval_deps(&mut bad_dice, &imports);
        let deps = LoadCycleDescriptor::guard_this(self.ctx, fut).await???;
        Ok((ast, deps))
    }

    pub fn prepare_eval_with_content<'a>(
        &'a self,
        starlark_file: StarlarkPath<'_>,
        content: String,
    ) -> anyhow::Result<ParseResult> {
        self.configs.parse(starlark_file, content)
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
    ) -> anyhow::Result<LoadedModule> {
        let (ast, deps) = self.prepare_eval(starlark_file.into()).await?;
        let loaded_modules = deps.get_loaded_modules();
        let buckconfig = self.get_legacy_buck_config_for_starlark().await?;
        let root_buckconfig = self.ctx.get_legacy_root_config_on_dice().await?;

        with_starlark_eval_provider(
            &mut self.ctx.bad_dice(),
            &mut StarlarkProfilerOrInstrumentation::disabled(),
            format!("load:{}", &starlark_file),
            move |provider, _| {
                let evaluation = self
                    .configs
                    .eval_module(
                        starlark_file,
                        &buckconfig,
                        &root_buckconfig,
                        ast,
                        loaded_modules.clone(),
                        provider,
                    )
                    .with_context(|| {
                        DiceCalculationDelegateError::EvalModuleError(starlark_file.to_string())
                    })?;

                Ok(LoadedModule::new(
                    OwnedStarlarkModulePath::new(starlark_file),
                    loaded_modules,
                    evaluation,
                ))
            },
        )
        .await
    }

    /// Eval parent `PACKAGE` file for given `PACKAGE` file.
    async fn eval_parent_package_file(
        &self,
        file: &PackageFilePath,
    ) -> anyhow::Result<SuperPackage> {
        let cell_resolver = self.ctx.bad_dice(/* configs */).get_cell_resolver().await?;
        let proj_rel_path = cell_resolver.resolve_path(file.dir())?;
        match proj_rel_path.parent() {
            None => {
                // We are in the project root, there's no parent.
                Ok(SuperPackage::empty::<SuperPackageValuesImpl>())
            }
            Some(parent) => {
                let parent_cell = cell_resolver.get_cell_path(parent)?;
                self.eval_package_file(&PackageFilePath::for_dir(parent_cell.as_ref()))
                    .await
            }
        }
    }

    /// Return `None` if there's no `PACKAGE` file in the directory.
    pub async fn prepare_package_file_eval(
        &self,
        path: &PackageFilePath,
    ) -> anyhow::Result<Option<(AstModule, ModuleDeps)>> {
        // This is cached if evaluating a `PACKAGE` file next to a `BUCK` file.
        let dir = DiceFileOps(self.ctx).read_dir(path.dir()).await?;
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
            None => {
                // If there's no `PACKAGE` file, return parent.
                return Ok(parent);
            }
        };

        let buckconfig = self.get_legacy_buck_config_for_starlark().await?;
        let root_buckconfig = self.ctx.get_legacy_root_config_on_dice().await?;
        with_starlark_eval_provider(
            &mut self.ctx.bad_dice(),
            &mut StarlarkProfilerOrInstrumentation::disabled(),
            format!("load:{}", path),
            move |provider, _| {
                self.configs
                    .eval_package_file(
                        path,
                        ast,
                        parent,
                        &buckconfig,
                        &root_buckconfig,
                        deps.get_loaded_modules(),
                        provider,
                    )
                    .with_context(|| format!("evaluating Starlark PACKAGE file `{}`", path))
            },
        )
        .await
    }

    pub(crate) async fn eval_package_file(
        &self,
        path: &PackageFilePath,
    ) -> anyhow::Result<SuperPackage> {
        #[derive(Debug, Display, Clone, Allocative, Eq, PartialEq, Hash)]
        struct PackageFileKey(PackageFilePath);

        #[async_trait]
        impl Key for PackageFileKey {
            type Value = buck2_error::Result<SuperPackage>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                let interpreter = ctx
                    .get_interpreter_calculator(self.0.cell(), self.0.build_file_cell())
                    .await?;
                interpreter
                    .eval_package_file_uncached(&self.0)
                    .await
                    .map_err(buck2_error::Error::from)
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
            .bad_dice(/* configs */)
            .compute(&PackageFileKey(path.clone()))
            .await?
            .map_err(anyhow::Error::from)
    }

    /// Most directories do not contain a `PACKAGE` file, this function
    /// optimizes `eval_package_file` for this case by avoiding creation of DICE key.
    pub(crate) async fn eval_package_file_for_build_file(
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
                let result = DicePackageListingResolver(&mut self.ctx.bad_dice(/* configs */))
                    .resolve_package_listing(package.dupe())
                    .await;
                (
                    result,
                    buck2_data::LoadPackageEnd {
                        path: package.as_cell_path().to_string(),
                    },
                )
            },
        )
        .await
    }

    // In order to prevent non deterministic crashes
    // we intentionally set off a starlark stack overflow, to make
    // sure that starlark catches the overflow and reports an error
    // before the native stack overflows
    async fn check_starlark_stack_size(&self) -> anyhow::Result<()> {
        #[derive(Debug, Display, Clone, Allocative, Eq, PartialEq, Hash)]
        struct StarlarkStackSizeChecker;

        #[async_trait]
        impl Key for StarlarkStackSizeChecker {
            type Value = buck2_error::Result<()>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                with_starlark_eval_provider(
                    ctx,
                    &mut StarlarkProfilerOrInstrumentation::disabled(),
                    "Check starlark stack size".to_owned(),
                    move |provider, _| {
                        let env = Module::new();
                        let (mut eval, _) = provider.make(&env)?;
                        let content = indoc!(
                            r#"
                                def f():
                                    f()
                                f()
                        "#
                        );
                        let ast =
                            AstModule::parse("x.star", content.to_owned(), &Dialect::Extended)
                                .map_err(BuckStarlarkError::new)?;
                        match eval.eval_module(ast, &Globals::standard()) {
                            Err(e) if e.to_string().contains("Starlark call stack overflow") => {
                                Ok(())
                            }
                            Err(p) => Err(BuckStarlarkError::new(p).into()),
                            Ok(_) => {
                                Err(DiceCalculationDelegateError::CheckStarlarkStackSizeError
                                    .into())
                            }
                        }
                    },
                )
                .await?;
                Ok(())
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
            .bad_dice(/* configs */)
            .compute(&StarlarkStackSizeChecker)
            .await?
            .map_err(anyhow::Error::from)
    }

    pub async fn eval_build_file(
        &self,
        package: PackageLabel,
        profiler_instrumentation: &mut StarlarkProfilerOrInstrumentation<'_>,
    ) -> buck2_error::Result<Arc<EvaluationResult>> {
        self.check_starlark_stack_size().await?;

        let listing = self.resolve_package_listing(package.dupe()).await?;

        let build_file_path = BuildFilePath::new(package.dupe(), listing.buildfile().to_owned());
        let ast_deps = self.prepare_eval(StarlarkPath::BuildFile(&build_file_path));

        let super_package = self.eval_package_file_for_build_file(package.dupe(), &listing);

        let ((ast, deps), super_package) = future::try_join(ast_deps, super_package).await?;

        let package_boundary_exception = self
            .ctx
            .bad_dice(/* configs */)
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
        with_starlark_eval_provider(
            &mut self.ctx.bad_dice(),
            profiler_instrumentation,
            format!("load_buildfile:{}", &package),
            move |provider, _| {
                span(start_event, move || {
                    let result_with_stats = self
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
                            provider,
                            false,
                        )
                        .with_context(|| {
                            DiceCalculationDelegateError::EvalBuildFileError(build_file_path)
                        });
                    let error = result_with_stats.as_ref().err().map(|e| format!("{:#}", e));
                    let starlark_peak_allocated_bytes = result_with_stats
                        .as_ref()
                        .map(|rs| rs.starlark_peak_allocated_bytes)
                        .unwrap_or(0);
                    let result = result_with_stats.map(|rs| rs.result);

                    (
                        result,
                        buck2_data::LoadBuildFileEnd {
                            module_id,
                            cell: cell_str,
                            starlark_peak_allocated_bytes,
                            error,
                        },
                    )
                })
            },
        )
        .await
        .map(Arc::new)
        .map_err(buck2_error::Error::from)
    }
}

mod keys {
    use allocative::Allocative;
    use buck2_interpreter::paths::module::OwnedStarlarkModulePath;
    use derive_more::Display;

    #[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    pub struct EvalImportKey(pub OwnedStarlarkModulePath);
}

pub mod testing {
    // re-exports for testing
    pub use super::keys::EvalImportKey;
}
