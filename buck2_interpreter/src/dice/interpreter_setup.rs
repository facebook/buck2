/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::LegacyBuckConfigs;
use buck2_core::cells::CellResolver;
use dice::DiceTransaction;

use crate::dice::starlark_profiler::SetStarlarkProfilerInstrumentation;
use crate::dice::starlark_types::SetDisableStarlarkTypes;
use crate::dice::HasInterpreterContext;
use crate::extra::InterpreterConfiguror;
use crate::starlark_profiler::StarlarkProfilerInstrumentation;

/// Common code to initialize Starlark interpreter globals.
pub fn setup_interpreter(
    dice: &DiceTransaction,
    cell_resolver: CellResolver,
    configuror: Arc<dyn InterpreterConfiguror>,
    legacy_configs: LegacyBuckConfigs,
    starlark_profiler_instrumentation_override: Option<StarlarkProfilerInstrumentation>,
    disable_starlark_types: bool,
) {
    dice.set_cell_resolver(cell_resolver);
    dice.set_interpreter_context(configuror);
    dice.set_legacy_configs(legacy_configs);
    dice.set_starlark_profiler_instrumentation_override(starlark_profiler_instrumentation_override);
    dice.set_disable_starlark_types(disable_starlark_types);
}

pub fn setup_interpreter_basic(
    dice: &DiceTransaction,
    cell_resolver: CellResolver,
    configuror: Arc<dyn InterpreterConfiguror>,
    legacy_configs: LegacyBuckConfigs,
) {
    setup_interpreter(dice, cell_resolver, configuror, legacy_configs, None, false);
}
