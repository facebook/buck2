/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Interpreter related Dice calculations

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::package_listing::dice::DicePackageListingResolver;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::package::PackageLabel;
use buck2_events::dispatch::async_record_root_spans;
use buck2_events::span::SpanId;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::file_loader::ModuleDeps;
use buck2_interpreter::load_module::INTERPRETER_CALCULATION_IMPL;
use buck2_interpreter::load_module::InterpreterCalculationImpl;
use buck2_interpreter::paths::module::OwnedStarlarkModulePath;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_interpreter::paths::path::OwnedStarlarkPath;
use buck2_interpreter::paths::path::StarlarkPath;
use buck2_interpreter::prelude_path::PreludePath;
use buck2_node::metadata::key::MetadataKey;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::frontend::TARGET_GRAPH_CALCULATION_IMPL;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::frontend::TargetGraphCalculationImpl;
use buck2_node::package_values_calculation::PACKAGE_VALUES_CALCULATION;
use buck2_node::package_values_calculation::PackageValuesCalculation;
use buck2_util::time_span::TimeSpan;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::FutureExt;
use futures::future::BoxFuture;
use smallvec::SmallVec;
use starlark::environment::Globals;
use starlark_map::small_map::SmallMap;

use crate::interpreter::dice_calculation_delegate::HasCalculationDelegate;
use crate::interpreter::dice_calculation_delegate::testing::EvalImportKey;
use crate::interpreter::global_interpreter_state::HasGlobalInterpreterState;
use crate::interpreter::package_file_calculation::EvalPackageFile;

// Key for 'InterpreterCalculation::get_interpreter_results'
#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
pub struct InterpreterResultsKey(pub PackageLabel);

struct TargetGraphCalculationInstance;

pub(crate) fn init_target_graph_calculation_impl() {
    TARGET_GRAPH_CALCULATION_IMPL.init(&TargetGraphCalculationInstance);
}

#[async_trait]
impl Key for InterpreterResultsKey {
    type Value = buck2_error::Result<Arc<EvaluationResult>>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        cancellation: &CancellationContext,
    ) -> Self::Value {
        let ((time_span, result), spans) = async_record_root_spans(
            ctx.get_interpreter_results_uncached(self.0.dupe(), cancellation),
        )
        .await;

        ctx.store_evaluation_data(InterpreterResultsKeyActivationData {
            time_span,
            result: result.dupe(),
            spans,
        })?;

        result
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        // TODO consider if we want to impl eq for this
        false
    }

    fn validity(x: &Self::Value) -> bool {
        x.is_ok()
    }
}

#[async_trait]
impl TargetGraphCalculationImpl for TargetGraphCalculationInstance {
    async fn get_interpreter_results_uncached(
        &self,
        ctx: &mut DiceComputations<'_>,
        package: PackageLabel,
        cancellation: &CancellationContext,
    ) -> (TimeSpan, buck2_error::Result<Arc<EvaluationResult>>) {
        match ctx
            .get_interpreter_calculator(OwnedStarlarkPath::PackageFile(
                PackageFilePath::package_file_for_dir(package.as_cell_path()),
            ))
            .await
        {
            Ok(mut interpreter) => {
                interpreter
                    .eval_build_file(package.dupe(), cancellation)
                    .await
            }
            Err(e) => (TimeSpan::empty_now(), Err(e)),
        }
    }

    fn get_interpreter_results<'a>(
        &self,
        ctx: &'a mut DiceComputations,
        package: PackageLabel,
    ) -> BoxFuture<'a, buck2_error::Result<Arc<EvaluationResult>>> {
        ctx.compute(&InterpreterResultsKey(package.dupe()))
            .map(|v| v?)
            .boxed()
    }
}

struct InterpreterCalculationInstance;
struct PackageValuesCalculationInstance;

pub(crate) fn init_interpreter_calculation_impl() {
    INTERPRETER_CALCULATION_IMPL.init(&InterpreterCalculationInstance);
    PACKAGE_VALUES_CALCULATION.init(&PackageValuesCalculationInstance);
}

#[async_trait]
impl Key for EvalImportKey {
    type Value = buck2_error::Result<LoadedModule>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        cancellation: &CancellationContext,
    ) -> Self::Value {
        let starlark_path = self.0.borrow();
        // We cannot just use the inner default delegate's eval_import
        // because that wouldn't delegate back to us for inner eval_import calls.
        Ok(ctx
            .get_interpreter_calculator(OwnedStarlarkPath::new(starlark_path.starlark_path()))
            .await?
            .eval_module_uncached(starlark_path, cancellation)
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

#[async_trait]
impl InterpreterCalculationImpl for InterpreterCalculationInstance {
    async fn get_loaded_module(
        &self,
        ctx: &mut DiceComputations<'_>,
        starlark_path: StarlarkModulePath<'_>,
    ) -> buck2_error::Result<LoadedModule> {
        ctx.compute(&EvalImportKey(OwnedStarlarkModulePath::new(starlark_path)))
            .await?
    }

    async fn get_module_deps(
        &self,
        ctx: &mut DiceComputations<'_>,
        package: PackageLabel,
    ) -> buck2_error::Result<ModuleDeps> {
        let build_file_name = DicePackageListingResolver(ctx)
            .resolve_package_listing(package.dupe())
            .await?
            .buildfile()
            .to_owned();

        let mut calc = ctx
            .get_interpreter_calculator(OwnedStarlarkPath::PackageFile(
                PackageFilePath::package_file_for_dir(package.as_cell_path()),
            ))
            .await?;

        let (_module, module_deps) = calc
            .prepare_eval(StarlarkPath::BuildFile(&BuildFilePath::new(
                package.dupe(),
                build_file_name,
            )))
            .await?;

        Ok(module_deps)
    }

    async fn get_package_file_deps(
        &self,
        ctx: &mut DiceComputations<'_>,
        package: PackageLabel,
    ) -> buck2_error::Result<Option<(PackageFilePath, Vec<ImportPath>)>> {
        // These aren't cached on the DICE graph, since in normal evaluation there aren't that many, and we can cache at a higher level.
        // Therefore we re-parse the file, if it exists.
        // Fortunately, there are only a small number (currently a few hundred)
        let mut interpreter = ctx
            .get_interpreter_calculator(OwnedStarlarkPath::PackageFile(
                PackageFilePath::package_file_for_dir(package.as_cell_path()),
            ))
            .await?;
        let x = interpreter.prepare_package_file_eval(package).await?;
        let Some((package_file_path, _module, deps)) = x else {
            return Ok(None);
        };
        Ok(Some((
            package_file_path,
            deps.get_loaded_modules().imports().cloned().collect(),
        )))
    }

    async fn global_env(&self, ctx: &mut DiceComputations<'_>) -> buck2_error::Result<Globals> {
        Ok(ctx.get_global_interpreter_state().await?.globals().dupe())
    }

    async fn prelude_import(
        &self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Option<PreludePath>> {
        Ok(ctx
            .get_global_interpreter_state()
            .await?
            .configuror
            .prelude_import()
            .cloned())
    }
}

#[async_trait]
impl PackageValuesCalculation for PackageValuesCalculationInstance {
    async fn package_values(
        &self,
        ctx: &mut DiceComputations<'_>,
        package: PackageLabel,
    ) -> buck2_error::Result<SmallMap<MetadataKey, serde_json::Value>> {
        ctx.eval_package_file(package)
            .await?
            .package_values()
            .package_values_json()
    }
}

pub struct InterpreterResultsKeyActivationData {
    /// TimeSpan of just the starlark evaluation of the build file.
    pub time_span: TimeSpan,
    pub result: buck2_error::Result<Arc<EvaluationResult>>,
    pub spans: SmallVec<[SpanId; 1]>,
}
