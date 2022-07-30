/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The dice module contains the interpreter's integration with dice. This
//! module contains the extension traits that we implement for Arc<DiceCtx> (the
//! implementations of the traits are in the submodules).
//!
//! Several of these extension traits provide implementations of our delegate/DI
//! traits that are themselves build on dice (ex DiceInterpreterFileOps
//! implements InterpreterFileOps by basically putting DefaultInterpreterFileOps
//! onto the dice graph).
use std::sync::Arc;

use buck2_common::dice::file_ops::FileChangeHandler;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::package_listing::dice::HasPackageListingResolver;
use buck2_common::result::SharedResult;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::CellName;
use dice::DiceComputations;
use dice::UserComputationData;
use events::dispatch::EventDispatcher;

use crate::dice::calculation::DiceCalculationDelegate;
use crate::extra::InterpreterConfiguror;
use crate::interpreter::GlobalInterpreterState;

pub mod calculation;
mod interpreter;
pub mod interpreter_setup;
pub mod starlark_profiler;
pub mod starlark_types;

#[async_trait]
pub(crate) trait HasInterpreterCalculations<'c>:
    HasFileOps<'c> + HasCalculationDelegate<'c> + HasPackageListingResolver<'c> + Send
{
}

impl<'c> HasInterpreterCalculations<'c> for DiceComputations {}

#[async_trait]
pub trait HasCalculationDelegate<'c> {
    /// Get calculator for a file evaluation.
    ///
    /// This function only accepts cell names, but it is created
    /// per evaluated file (build file or `.bzl`).
    async fn get_interpreter_calculator(
        &'c self,
        cell: &CellName,
        build_file_cell: &BuildFileCell,
    ) -> anyhow::Result<DiceCalculationDelegate<'c>>;
}

#[async_trait]
pub trait HasGlobalInterpreterState {
    async fn get_global_interpreter_state(&self) -> SharedResult<Arc<GlobalInterpreterState>>;
}

#[async_trait]
pub trait HasInterpreterContext {
    fn get_file_change_handler(&self) -> &dyn FileChangeHandler;

    async fn get_interpreter_configuror(&self) -> anyhow::Result<Arc<dyn InterpreterConfiguror>>;

    fn set_interpreter_context(
        &self,
        interpreter_configuror: Arc<dyn InterpreterConfiguror>,
    ) -> anyhow::Result<()>;
}

pub trait HasEvents {
    fn get_dispatcher(&self) -> &EventDispatcher;
}

impl HasEvents for UserComputationData {
    fn get_dispatcher(&self) -> &EventDispatcher {
        self.data
            .get::<EventDispatcher>()
            .expect("Event dispatcher should be set")
    }
}

pub mod testing {
    pub use crate::dice::calculation::testing::EvalImportKey;
}
