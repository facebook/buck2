/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::dice::cells::SetCellResolver;
use buck2_common::legacy_configs::dice::SetLegacyConfigs;
use buck2_common::legacy_configs::LegacyBuckConfigs;
use buck2_core::cells::CellResolver;
use buck2_interpreter::dice::starlark_profiler::SetStarlarkProfilerInstrumentation;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_interpreter::dice::starlark_types::SetDisableStarlarkTypes;
use buck2_interpreter::extra::InterpreterConfiguror;
use dice::DiceTransactionUpdater;

use crate::interpreter::context::SetInterpreterContext;

/// Common code to initialize Starlark interpreter globals.
pub fn setup_interpreter(
    updater: &mut DiceTransactionUpdater,
    cell_resolver: CellResolver,
    configuror: Arc<dyn InterpreterConfiguror>,
    legacy_configs: LegacyBuckConfigs,
    starlark_profiler_instrumentation_override: StarlarkProfilerConfiguration,
    disable_starlark_types: bool,
) -> anyhow::Result<()> {
    updater.set_cell_resolver(cell_resolver)?;
    updater.set_interpreter_context(configuror)?;
    updater.set_legacy_configs(legacy_configs)?;
    updater.set_starlark_profiler_instrumentation_override(
        starlark_profiler_instrumentation_override,
    )?;
    updater.set_disable_starlark_types(disable_starlark_types)?;

    Ok(())
}

pub fn setup_interpreter_basic(
    dice: &mut DiceTransactionUpdater,
    cell_resolver: CellResolver,
    configuror: Arc<dyn InterpreterConfiguror>,
    legacy_configs: LegacyBuckConfigs,
) -> anyhow::Result<()> {
    setup_interpreter(
        dice,
        cell_resolver,
        configuror,
        legacy_configs,
        StarlarkProfilerConfiguration::default(),
        false,
    )
}
