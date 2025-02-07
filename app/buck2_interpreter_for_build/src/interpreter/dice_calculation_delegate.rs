/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::cycles::CycleGuard;
use buck2_common::dice::file_ops::DiceFileComputations;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::dice::OpaqueLegacyBuckConfigOnDice;
use buck2_common::package_boundary::HasPackageBoundaryExceptions;
use buck2_common::package_listing::dice::DicePackageListingResolver;
use buck2_common::package_listing::listing::PackageListing;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::name::CellName;
use buck2_core::package::PackageLabel;
use buck2_error::internal_error;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::span;
use buck2_events::dispatch::span_async_simple;
use buck2_futures::cancellation::CancellationContext;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::file_loader::ModuleDeps;
use buck2_interpreter::import_paths::HasImportPaths;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::paths::module::OwnedStarlarkModulePath;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_interpreter::paths::path::StarlarkPath;
use buck2_interpreter::starlark_profiler::config::GetStarlarkProfilerInstrumentation;
use buck2_interpreter::starlark_profiler::profiler::StarlarkProfilerOpt;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::super_package::SuperPackage;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::FutureExt;
use starlark::codemap::FileSpan;
use starlark::syntax::AstModule;

use crate::interpreter::buckconfig::ConfigsOnDiceViewForStarlark;
use crate::interpreter::cell_info::InterpreterCellInfo;
use crate::interpreter::check_starlark_stack_size::check_starlark_stack_size;
use crate::interpreter::cycles::LoadCycleDescriptor;
use crate::interpreter::global_interpreter_state::HasGlobalInterpreterState;
use crate::interpreter::interpreter_for_cell::InterpreterForCell;
use crate::interpreter::interpreter_for_cell::ParseData;
use crate::interpreter::interpreter_for_cell::ParseResult;
use crate::super_package::package_value::SuperPackageValuesImpl;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum DiceCalculationDelegateError {
    #[error("Error evaluating build file: `{0}`")]
    EvalBuildFileError(BuildFilePath),
    #[error("Error evaluating module: `{0}`")]
    EvalModuleError(String),
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
    ) -> buck2_error::Result<DiceCalculationDelegate<'c, 'd>>;
}

#[async_trait]
impl<'c, 'd> HasCalculationDelegate<'c, 'd> for DiceComputations<'d> {
    async fn get_interpreter_calculator(
        &'c mut self,
        cell: CellName,
        build_file_cell: BuildFileCell,
    ) -> buck2_error::Result<DiceCalculationDelegate<'c, 'd>> {
        #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
        #[display("{}@{}", _0, _1)]
        struct InterpreterConfigForCellKey(CellName, BuildFileCell);

        #[async_trait]
        impl Key for InterpreterConfigForCellKey {
            type Value = buck2_error::Result<Arc<InterpreterForCell>>;
            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                let global_state = ctx.get_global_interpreter_state().await?;

                let cell_alias_resolver = ctx.get_cell_alias_resolver(self.0).await?;

                let implicit_import_paths = ctx.import_paths_for_cell(self.1).await?;

                let cell_info = InterpreterCellInfo::new(
                    self.1,
                    ctx.get_cell_resolver().await?,
                    cell_alias_resolver,
                )?;

                Ok(Arc::new(InterpreterForCell::new(
                    cell_info,
                    global_state.dupe(),
                    implicit_import_paths,
                )?))
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                false
            }
        }

        let configs = self
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
        &mut self,
    ) -> buck2_error::Result<OpaqueLegacyBuckConfigOnDice> {
        self.ctx
            .get_legacy_config_on_dice(self.build_file_cell.name())
            .await
    }

    async fn parse_file(
        &mut self,
        starlark_path: StarlarkPath<'_>,
    ) -> buck2_error::Result<ParseResult> {
        let content =
            DiceFileComputations::read_file(self.ctx, starlark_path.path().as_ref().as_ref())
                .await?;
        self.configs.parse(starlark_path, content)
    }

    async fn eval_deps(
        ctx: &mut DiceComputations<'_>,
        modules: &[(Option<FileSpan>, OwnedStarlarkModulePath)],
    ) -> buck2_error::Result<ModuleDeps> {
        Ok(ModuleDeps(
            ctx.try_compute_join(modules, |ctx, (span, import)| {
                async move {
                    ctx.get_loaded_module(import.borrow())
                        .await
                        .with_buck_error_context(|| {
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
        &'a mut self,
        starlark_file: StarlarkPath<'_>,
    ) -> buck2_error::Result<(AstModule, ModuleDeps)> {
        let ParseData(ast, imports) = self.parse_file(starlark_file).await??;
        let deps = CycleGuard::<LoadCycleDescriptor>::new(self.ctx)?
            .guard_this(Self::eval_deps(self.ctx, &imports))
            .await
            .into_result(self.ctx)
            .await???;
        Ok((ast, deps))
    }

    pub fn prepare_eval_with_content<'a>(
        &'a self,
        starlark_file: StarlarkPath<'_>,
        content: String,
    ) -> buck2_error::Result<ParseResult> {
        self.configs.parse(starlark_file, content)
    }

    pub async fn resolve_load(
        &self,
        starlark_file: StarlarkPath<'_>,
        load_string: &str,
    ) -> buck2_error::Result<OwnedStarlarkModulePath> {
        self.configs.resolve_path(starlark_file, load_string)
    }

    pub async fn eval_module_uncached(
        &mut self,
        starlark_file: StarlarkModulePath<'_>,
    ) -> buck2_error::Result<LoadedModule> {
        let (ast, deps) = self.prepare_eval(starlark_file.into()).await?;
        let loaded_modules = deps.get_loaded_modules();
        let buckconfig = self.get_legacy_buck_config_for_starlark().await?;
        let root_buckconfig = self.ctx.get_legacy_root_config_on_dice().await?;

        let configs = &self.configs;
        let ctx = &mut *self.ctx;

        with_starlark_eval_provider(
            ctx,
            &mut StarlarkProfilerOpt::disabled(),
            &StarlarkEvalKind::Load(Arc::new(starlark_file.to_owned())),
            move |provider, ctx| {
                let mut buckconfigs =
                    ConfigsOnDiceViewForStarlark::new(ctx, buckconfig, root_buckconfig);
                let evaluation = configs
                    .eval_module(
                        starlark_file,
                        &mut buckconfigs,
                        ast,
                        loaded_modules.clone(),
                        provider,
                    )
                    .with_buck_error_context(|| {
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

    /// Eval parent `PACKAGE` file for given package file.
    async fn eval_parent_package_file(
        &mut self,
        file: PackageLabel,
    ) -> buck2_error::Result<SuperPackage> {
        let cell_resolver = self.ctx.get_cell_resolver().await?;
        let proj_rel_path = cell_resolver.resolve_path(file.as_cell_path())?;
        match proj_rel_path.parent() {
            None => {
                // We are in the project root, there's no parent.
                Ok(SuperPackage::empty::<SuperPackageValuesImpl>()?)
            }
            Some(parent) => {
                let parent_cell = cell_resolver.get_cell_path(parent)?;
                self.eval_package_file(PackageLabel::from_cell_path(parent_cell.as_ref()))
                    .await
            }
        }
    }

    /// Return `None` if there's no `PACKAGE` file in the directory.
    pub async fn prepare_package_file_eval(
        &mut self,
        package: PackageLabel,
    ) -> buck2_error::Result<Option<(PackageFilePath, AstModule, ModuleDeps)>> {
        // Note:
        // * we are using `read_dir` instead of `read_path_metadata` because
        //   * it is an extra IO, and `read_dir` is likely already cached.
        //   * `read_path_metadata` would not tell us if the file name is `PACKAGE`
        //     and not `package` on case-insensitive filesystems.
        //     We do case-sensitive comparison for `BUCK` files, so we do the same here.
        //   * we fail here if `PACKAGE` (but not `package`) exists, and it is not a file.

        // package file results capture starlark values and so cannot be checked for equality. This means we
        // can't get early cutoff for the consumers, and so we need to be careful to ensure our deps are precise.
        // Otherwise noop package value recomputations can lead to large recompute costs.
        //
        // Here we put the package file check behind an additional dice key so that we don't recompute on irrelevant
        // changes to the directory contents.
        #[derive(Debug, Display, Clone, Allocative, Eq, PartialEq, Hash)]
        struct PackageFileLookupKey(PackageLabel);

        #[async_trait]
        impl Key for PackageFileLookupKey {
            type Value = buck2_error::Result<Option<Arc<PackageFilePath>>>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                // This is cached if evaluating a `PACKAGE` file next to a `BUCK` file.
                let dir = DiceFileComputations::read_dir(ctx, self.0.as_cell_path()).await?;
                for package_file_path in PackageFilePath::for_dir(self.0.as_cell_path()) {
                    if !dir.contains(
                        package_file_path
                            .path()
                            .path()
                            .file_name()
                            .internal_error("Must have name")?,
                    ) {
                        continue;
                    }
                    return Ok(Some(Arc::new(package_file_path)));
                }
                Ok(None)
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

        match self
            .ctx
            .compute(&PackageFileLookupKey(package.dupe()))
            .await??
        {
            Some(package_file_path) => {
                let (module, deps) = self
                    .prepare_eval(StarlarkPath::PackageFile(&package_file_path))
                    .await?;
                Ok(Some(((*package_file_path).clone(), module, deps)))
            }
            None => Ok(None),
        }
    }

    async fn eval_package_file_uncached(
        &mut self,
        path: PackageLabel,
    ) -> buck2_error::Result<SuperPackage> {
        let parent = self.eval_parent_package_file(path.dupe()).await?;
        let ast_deps = self.prepare_package_file_eval(path.dupe()).await?;

        let (package_file_path, ast, deps) = match ast_deps {
            Some(x) => x,
            None => {
                // If there's no `PACKAGE` file, return parent.
                return Ok(parent);
            }
        };

        let buckconfig = self.get_legacy_buck_config_for_starlark().await?;
        let root_buckconfig = self.ctx.get_legacy_root_config_on_dice().await?;

        let configs = &self.configs;
        let ctx = &mut *self.ctx;

        with_starlark_eval_provider(
            ctx,
            &mut StarlarkProfilerOpt::disabled(),
            &StarlarkEvalKind::LoadPackageFile(path.dupe()),
            move |provider, ctx| {
                let mut buckconfigs =
                    ConfigsOnDiceViewForStarlark::new(ctx, buckconfig, root_buckconfig);

                configs
                    .eval_package_file(
                        &package_file_path,
                        ast,
                        parent,
                        &mut buckconfigs,
                        deps.get_loaded_modules(),
                        provider,
                    )
                    .with_buck_error_context(|| {
                        format!("evaluating Starlark PACKAGE file `{}`", path)
                    })
            },
        )
        .await
    }

    pub(crate) async fn eval_package_file(
        &mut self,
        path: PackageLabel,
    ) -> buck2_error::Result<SuperPackage> {
        #[derive(Debug, Display, Clone, Allocative, Eq, PartialEq, Hash)]
        struct PackageFileKey(PackageLabel);

        #[async_trait]
        impl Key for PackageFileKey {
            type Value = buck2_error::Result<SuperPackage>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                let cell_name = self.0.as_cell_path().cell();
                let mut interpreter = ctx
                    .get_interpreter_calculator(cell_name, BuildFileCell::new(cell_name))
                    .await?;
                interpreter
                    .eval_package_file_uncached(self.0.dupe())
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
            .compute(&PackageFileKey(path))
            .await?
            .map_err(buck2_error::Error::from)
    }

    /// Most directories do not contain a `PACKAGE` file, this function
    /// optimizes `eval_package_file` for this case by avoiding creation of DICE key.
    pub(crate) async fn eval_package_file_for_build_file(
        &mut self,
        package: PackageLabel,
        package_listing: &PackageListing,
    ) -> buck2_error::Result<SuperPackage> {
        for package_file_name in PackageFilePath::package_file_names() {
            if package_listing
                .get_file(package_file_name.as_ref())
                .is_some()
            {
                return self.eval_package_file(package).await;
            }
        }

        // Without this optimization, `cquery <that android target>` has 6% time regression.
        // With this optimization, check for `PACKAGE` files adds 2% to time.
        self.eval_parent_package_file(package).await
    }

    async fn resolve_package_listing(
        ctx: &mut DiceComputations<'_>,
        package: PackageLabel,
    ) -> buck2_error::Result<PackageListing> {
        span_async_simple(
            buck2_data::LoadPackageStart {
                path: package.as_cell_path().to_string(),
            },
            DicePackageListingResolver(ctx).resolve_package_listing(package.dupe()),
            buck2_data::LoadPackageEnd {
                path: package.as_cell_path().to_string(),
            },
        )
        .await
    }

    pub async fn eval_build_file(
        &mut self,
        package: PackageLabel,
    ) -> (Duration, buck2_error::Result<Arc<EvaluationResult>>) {
        let mut now = None;
        let eval_kind = StarlarkEvalKind::LoadBuildFile(package.dupe());
        let eval_result: buck2_error::Result<_> = try {
            let ((), listing, mut profiler) = self
                .ctx
                .try_compute3(
                    |ctx| check_starlark_stack_size(ctx).boxed(),
                    |ctx| Self::resolve_package_listing(ctx, package.dupe()).boxed(),
                    |ctx| ctx.get_starlark_profiler(&eval_kind),
                )
                .await?;

            let build_file_path =
                BuildFilePath::new(package.dupe(), listing.buildfile().to_owned());
            let (ast, deps) = self
                .prepare_eval(StarlarkPath::BuildFile(&build_file_path))
                .await?;
            let super_package = self
                .eval_package_file_for_build_file(package.dupe(), &listing)
                .await?;

            let package_boundary_exception = self
                .ctx
                .get_package_boundary_exception(package.as_cell_path())
                .await?
                .is_some();
            let buckconfig = self.get_legacy_buck_config_for_starlark().await?;
            let root_buckconfig = self.ctx.get_legacy_root_config_on_dice().await?;
            let module_id = build_file_path.to_string();
            let cell_str = build_file_path.cell().as_str().to_owned();
            let start_event = buck2_data::LoadBuildFileStart {
                cell: cell_str.clone(),
                module_id: module_id.clone(),
            };

            let configs = &self.configs;
            let ctx = &mut *self.ctx;

            now = Some(Instant::now());
            let mut eval_result = with_starlark_eval_provider(
                ctx,
                &mut profiler.as_mut(),
                &eval_kind,
                move |provider, ctx| {
                    let mut buckconfigs =
                        ConfigsOnDiceViewForStarlark::new(ctx, buckconfig, root_buckconfig);

                    span(start_event, move || {
                        let result_with_stats = configs
                            .eval_build_file(
                                &build_file_path,
                                &mut buckconfigs,
                                listing,
                                super_package,
                                package_boundary_exception,
                                ast,
                                deps.get_loaded_modules(),
                                provider,
                                false,
                            )
                            .with_buck_error_context(|| {
                                DiceCalculationDelegateError::EvalBuildFileError(build_file_path)
                            });
                        let error = result_with_stats.as_ref().err().map(|e| format!("{:#}", e));
                        let starlark_peak_allocated_bytes = result_with_stats
                            .as_ref()
                            .ok()
                            .map(|rs| rs.starlark_peak_allocated_bytes);
                        let cpu_instruction_count = result_with_stats
                            .as_ref()
                            .ok()
                            .and_then(|rs| rs.cpu_instruction_count);
                        let result = result_with_stats.map(|rs| rs.result);
                        let target_count = result.as_ref().ok().map(|rs| rs.targets().len() as u64);

                        (
                            result,
                            buck2_data::LoadBuildFileEnd {
                                module_id,
                                cell: cell_str,
                                target_count,
                                starlark_peak_allocated_bytes,
                                cpu_instruction_count,
                                error,
                            },
                        )
                    })
                },
            )
            .await?;
            let profile_data = profiler.finish(None)?;
            if eval_result.starlark_profile.is_some() {
                return (
                    now.unwrap().elapsed(),
                    Err(internal_error!("starlark_profile field must not be set yet").into()),
                );
            }
            eval_result.starlark_profile = profile_data.map(|d| Arc::new(d) as _);
            eval_result
        };

        (
            now.map_or(Duration::ZERO, |v| v.elapsed()),
            eval_result.map(Arc::new),
        )
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
