/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{collections::HashMap, sync::Arc};

use buck2_common::{
    dice::{
        cells::HasCellResolver,
        file_ops::{DiceFileOps, HasFileOps},
    },
    file_ops::FileOps,
    legacy_configs::dice::{HasLegacyConfigs, LegacyBuckConfigOnDice},
    package_boundary::HasPackageBoundaryExceptions,
};
use buck2_core::{
    cells::{paths::CellPath, CellName},
    package::Package,
    result::SharedResult,
};
use derive_more::Display;
use dice::{DiceComputations, Key};
use events::dispatch::EventDispatcher;
use gazebo::prelude::*;
use starlark::syntax::AstModule;

use crate::{
    common::{
        BuildFileCell, BuildFilePath, ImportPath, OwnedStarlarkModulePath, StarlarkModulePath,
        StarlarkPath,
    },
    dice::{
        calculation::keys::EvalImportKey, starlark_profiler::GetStarlarkProfilerInstrumentation,
        starlark_types::GetDisableStarlarkTypes, HasCalculationDelegate, HasEvents,
        HasGlobalInterpreterState, HasInterpreterContext, HasPackageListingResolver,
    },
    extra::ExtraContext,
    file_loader::{LoadedModule, ModuleDeps},
    import_paths::HasImportPaths,
    interpreter::{
        GlobalInterpreterState, InterpreterConfigForCell, InterpreterForCell, ParseResult,
    },
    package_listing::{listing::PackageListing, resolver::PackageListingResolver},
    starlark_profiler::{StarlarkProfilerInstrumentation, StarlarkProfilerOrInstrumentation},
};

#[async_trait]
impl<'c> HasCalculationDelegate<'c> for DiceComputations {
    async fn get_interpreter_calculator(
        &'c self,
        cell: &CellName,
        build_file_cell: &BuildFileCell,
    ) -> anyhow::Result<DiceCalculationDelegate<'c>> {
        #[derive(Clone, Dupe)]
        struct ConfigsValue(Arc<HashMap<CellName, Arc<InterpreterConfigForCell>>>);

        #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
        #[display(fmt = "{:?}", self)]
        struct ConfigsKey();

        #[async_trait]
        impl Key for ConfigsKey {
            type Value = SharedResult<ConfigsValue>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let cell_resolver = ctx.get_cell_resolver().await;
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
        let configs = self.compute(&ConfigsKey()).await?;

        Ok(DiceCalculationDelegate {
            build_file_cell: build_file_cell.clone(),
            ctx: self,
            fs: file_ops,
            configs: configs
                .0
                .get(cell)
                .expect("Should be impossible to get a cell without a config.")
                .dupe(),
            event_dispatcher: self.per_transaction_data().get_dispatcher().dupe(),
        })
    }
}

#[async_trait]
impl HasGlobalInterpreterState for DiceComputations {
    async fn get_global_interpreter_state(&self) -> SharedResult<Arc<GlobalInterpreterState>> {
        #[derive(Clone, Dupe)]
        struct GisValue(Arc<GlobalInterpreterState>);

        #[derive(Clone, Display, Dupe, Debug, Eq, Hash, PartialEq)]
        #[display(fmt = "{:?}", self)]
        struct GisKey();

        #[async_trait]
        impl Key for GisKey {
            type Value = SharedResult<GisValue>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let interpreter_configuror = ctx.get_interpreter_configuror().await;
                let legacy_configs = ctx.get_legacy_configs_on_dice().await;
                let cell_resolver = ctx.get_cell_resolver().await;
                let disable_starlark_types = ctx.get_disable_starlark_types().await;

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

        Ok(self.compute(&GisKey()).await?.0)
    }
}

#[derive(Clone)]
pub struct DiceCalculationDelegate<'c> {
    build_file_cell: BuildFileCell,
    ctx: &'c DiceComputations,
    fs: DiceFileOps<'c>,
    configs: Arc<InterpreterConfigForCell>,
    event_dispatcher: EventDispatcher,
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
                ctx.get_interpreter_calculator(
                    starlark_path.cell(),
                    starlark_path.build_file_cell(),
                )
                .await?
                .eval_module_uncached(starlark_path, starlark_profiler_instrumentation)
                .await
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                // While it is technically possible to compare the modules
                // at least for simple modules (like modules defining only string constants),
                // practically it is too hard to make it work correctly for every case.
                false
            }
        }

        self.ctx
            .compute(&EvalImportKey(OwnedStarlarkModulePath::new(starlark_path)))
            .await
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
            .await
            .get(self.build_file_cell.name())
    }

    fn event_dispatcher(&self) -> &EventDispatcher {
        &self.event_dispatcher
    }

    async fn get_package_boundary_exception(&self, path: &CellPath) -> SharedResult<bool> {
        self.ctx.get_package_boundary_exception(path).await
    }

    async fn parse_file(&self, starlark_path: StarlarkPath<'_>) -> anyhow::Result<ParseResult> {
        let content = self
            .get_file_ops()
            .read_file(&*starlark_path.path())
            .await?;
        self.get_interpreter_for_cell()
            .await?
            .parse(starlark_path, content)
    }

    async fn eval_deps(&self, modules: &[ImportPath]) -> SharedResult<ModuleDeps> {
        Ok(ModuleDeps(
            futures::future::join_all(
                modules
                    .iter()
                    .map(|import| self.eval_module(StarlarkModulePath::LoadFile(import))),
            )
            .await
            .into_iter()
            .collect::<SharedResult<_>>()?,
        ))
    }

    async fn prepare_eval<'a>(
        &'a self,
        starlark_file: StarlarkPath<'_>,
    ) -> SharedResult<(AstModule, ModuleDeps)> {
        let ParseResult(ast, imports) = self.parse_file(starlark_file).await?;
        let deps = self.eval_deps(&*imports).await?;
        Ok((ast, deps))
    }

    pub(crate) async fn eval_module_uncached(
        &self,
        starlark_file: StarlarkModulePath<'_>,
        starlark_profiler_instrumentation: StarlarkProfilerInstrumentation,
    ) -> SharedResult<LoadedModule> {
        let (ast, deps) = self.prepare_eval(starlark_file.into()).await?;
        let loaded_modules = deps.get_loaded_modules();
        let imports = LoadedModule::imports_from_loaded_modules(&loaded_modules);
        let buckconfig = self.get_legacy_buck_config_for_starlark().await?;

        self.get_interpreter_for_cell()
            .await?
            .eval_module(
                starlark_file,
                &buckconfig,
                ast,
                loaded_modules,
                starlark_profiler_instrumentation,
            )
            .map(|e| LoadedModule::new(OwnedStarlarkModulePath::new(starlark_file), imports, e))
    }

    pub async fn eval_build_file<T: ExtraContext>(
        &self,
        package: &Package,
        profiler: &mut StarlarkProfilerOrInstrumentation<'_>,
    ) -> SharedResult<T::EvalResult> {
        let listing = self
            .event_dispatcher()
            .span_async(
                buck2_data::LoadPackageStart {
                    path: package.as_cell_path().to_string(),
                },
                async {
                    (
                        self.resolve_package_listing(package).await,
                        buck2_data::LoadPackageEnd {
                            path: package.as_cell_path().to_string(),
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
        self.event_dispatcher().span(start_event, move || {
            let result = try {
                interpreter.eval_build_file::<T>(
                    &build_file_path,
                    &buckconfig,
                    listing,
                    package_boundary_exception,
                    ast,
                    deps.get_loaded_modules(),
                    profiler,
                )?
            };

            (
                result,
                buck2_data::LoadBuildFileEnd {
                    module_id,
                    cell: cell_str,
                },
            )
        })
    }
}

mod keys {
    use derive_more::Display;

    use crate::common::OwnedStarlarkModulePath;

    #[derive(Clone, Display, Debug, Eq, Hash, PartialEq)]
    #[display(fmt = "{}", .0)]
    pub struct EvalImportKey(pub OwnedStarlarkModulePath);
}

pub mod testing {
    // re-exports for testing
    pub use super::keys::EvalImportKey;
}
