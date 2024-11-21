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
use buck2_common::legacy_configs::cells::ExternalBuckconfigData;
use buck2_common::legacy_configs::dice::SetLegacyConfigs;
use buck2_core::cells::CellResolver;
use buck2_interpreter::dice::starlark_types::SetStarlarkTypes;
use buck2_interpreter::starlark_profiler::config::SetStarlarkProfilerInstrumentation;
use buck2_interpreter::starlark_profiler::config::StarlarkProfilerConfiguration;
use dice::DiceTransactionUpdater;

use crate::interpreter::configuror::BuildInterpreterConfiguror;
use crate::interpreter::context::SetInterpreterContext;

/// Common code to initialize Starlark interpreter globals.
pub fn setup_interpreter(
    updater: &mut DiceTransactionUpdater,
    cell_resolver: CellResolver,
    configuror: Arc<BuildInterpreterConfiguror>,
    legacy_config_overrides: Arc<ExternalBuckconfigData>,
    starlark_profiler_instrumentation_override: StarlarkProfilerConfiguration,
    disable_starlark_types: bool,
    unstable_typecheck: bool,
) -> buck2_error::Result<()> {
    updater.set_cell_resolver(cell_resolver)?;
    updater.set_interpreter_context(configuror)?;
    updater.set_legacy_config_external_data(legacy_config_overrides)?;
    updater.set_starlark_profiler_configuration(starlark_profiler_instrumentation_override)?;
    updater.set_starlark_types(disable_starlark_types, unstable_typecheck)?;

    Ok(())
}

pub fn setup_interpreter_basic(
    dice: &mut DiceTransactionUpdater,
    cell_resolver: CellResolver,
    configuror: Arc<BuildInterpreterConfiguror>,
) -> buck2_error::Result<()> {
    setup_interpreter(
        dice,
        cell_resolver,
        configuror,
        Arc::new(ExternalBuckconfigData::testing_default()),
        StarlarkProfilerConfiguration::default(),
        false,
        false,
    )
}
