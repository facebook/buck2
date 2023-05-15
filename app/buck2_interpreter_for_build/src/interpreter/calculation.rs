/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Interpreter related Dice calculations

use std::sync::Arc;

use async_trait::async_trait;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_common::result::ToUnsharedResultExt;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::package::PackageLabel;
use buck2_interpreter::dice::starlark_profiler::GetStarlarkProfilerInstrumentation;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::path::StarlarkModulePath;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::frontend::TargetGraphCalculationImpl;
use buck2_node::nodes::frontend::TARGET_GRAPH_CALCULATION_IMPL;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use more_futures::cancellation::CancellationContext;

use crate::interpreter::calculation::keys::InterpreterResultsKey;
use crate::interpreter::dice_calculation_delegate::HasCalculationDelegate;

struct TargetGraphCalculationInstance;

pub(crate) fn init_target_graph_calculation_impl() {
    TARGET_GRAPH_CALCULATION_IMPL.init(&TargetGraphCalculationInstance);
}

#[async_trait]
impl TargetGraphCalculationImpl for TargetGraphCalculationInstance {
    async fn get_interpreter_results_uncached(
        &self,
        ctx: &DiceComputations,
        package: PackageLabel,
    ) -> anyhow::Result<Arc<EvaluationResult>> {
        let starlark_profiler_instrumentation = ctx.get_starlark_profiler_instrumentation().await?;
        let interpreter = ctx
            .get_interpreter_calculator(
                package.cell_name(),
                BuildFileCell::new(package.cell_name()),
            )
            .await?;
        Ok(Arc::new(
            interpreter
                .eval_build_file(
                    package.dupe(),
                    &mut StarlarkProfilerOrInstrumentation::maybe_instrumentation(
                        starlark_profiler_instrumentation,
                    ),
                )
                .await?,
        ))
    }

    async fn get_interpreter_results(
        &self,
        ctx: &DiceComputations,
        package: PackageLabel,
    ) -> anyhow::Result<Arc<EvaluationResult>> {
        #[async_trait]
        impl Key for InterpreterResultsKey {
            type Value = SharedResult<Arc<EvaluationResult>>;
            async fn compute(
                &self,
                ctx: &DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                ctx.get_interpreter_results_uncached(self.0.dupe())
                    .await
                    .shared_error()
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                // TODO consider if we want to impl eq for this
                false
            }

            fn validity(x: &Self::Value) -> bool {
                x.is_ok()
            }
        }

        ctx.compute(&InterpreterResultsKey(package.dupe()))
            .await?
            .unshared_error()
    }
}

#[async_trait]
pub trait InterpreterCalculation {
    /// Returns the LoadedModule for a given starlark file. This is cached on the dice graph.
    async fn get_loaded_module(&self, path: StarlarkModulePath<'_>)
    -> anyhow::Result<LoadedModule>;

    async fn get_loaded_module_from_import_path(
        &self,
        path: &ImportPath,
    ) -> anyhow::Result<LoadedModule>;
}

#[async_trait]
impl InterpreterCalculation for DiceComputations {
    async fn get_loaded_module(
        &self,
        path: StarlarkModulePath<'_>,
    ) -> anyhow::Result<LoadedModule> {
        self.get_interpreter_calculator(path.cell(), path.build_file_cell())
            .await?
            .eval_module(path)
            .await
    }

    async fn get_loaded_module_from_import_path(
        &self,
        path: &ImportPath,
    ) -> anyhow::Result<LoadedModule> {
        let module_path = StarlarkModulePath::LoadFile(path);
        self.get_loaded_module(module_path).await
    }
}

mod keys {
    use allocative::Allocative;
    use buck2_core::package::PackageLabel;
    use derive_more::Display;
    use dupe::Dupe;

    // Key for 'InterpreterCalculation::get_interpreter_results'
    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    pub struct InterpreterResultsKey(pub PackageLabel);
}

pub mod testing {
    // re-exports for testing
    pub use super::keys::InterpreterResultsKey;
}
