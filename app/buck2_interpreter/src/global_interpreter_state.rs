/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use allocative::Allocative;
use buck2_common::legacy_configs::view::LegacyBuckConfigsView;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::CellResolver;
use dupe::Dupe;
use starlark::environment::Globals;

use crate::extra::cell_info::InterpreterCellInfo;
use crate::extra::InterpreterConfiguror;

/// Information shared across interpreters. Contains no cell-specific
/// information.
#[derive(Allocative)]
pub struct GlobalInterpreterState {
    pub(crate) cell_resolver: CellResolver,

    pub(crate) cell_configs: HashMap<BuildFileCell, InterpreterCellInfo>,

    /// The GlobalEnvironment contains all the globally available symbols
    /// (primarily starlark stdlib and Buck-provided functions) that should
    /// be available in a build file.
    pub build_file_global_env: Globals,

    /// The GlobalEnvironment contains all the globally available symbols
    /// (primarily starlark stdlib and Buck-provided functions) that should
    /// be available in an extension file.
    pub extension_file_global_env: Globals,

    /// The GlobalEnvironment contains all the globally available symbols
    /// (primarily starlark stdlib and Buck-provided functions) that should
    /// be available in a bxl file.
    pub bxl_file_global_env: Globals,

    /// Interpreter Configurer
    pub(crate) configuror: Arc<dyn InterpreterConfiguror>,

    /// Check types in Starlark (or just parse and ignore).
    pub(crate) disable_starlark_types: bool,
}

impl GlobalInterpreterState {
    pub fn new(
        legacy_configs: &dyn LegacyBuckConfigsView,
        cell_resolver: CellResolver,
        interpreter_configuror: Arc<dyn InterpreterConfiguror>,
        disable_starlark_types: bool,
    ) -> anyhow::Result<Self> {
        // TODO: There should be one of these that also does not have native functions
        // in the global       namespace so that it can be configured per-cell
        let build_file_global_env = interpreter_configuror.build_file_globals();
        let extension_file_global_env = interpreter_configuror.extension_file_globals();
        let bxl_file_global_env = interpreter_configuror.bxl_file_globals();

        let mut cell_configs = HashMap::new();
        for (cell_name, config) in legacy_configs.iter() {
            let cell_instance = cell_resolver.get(cell_name).expect("Should have cell.");
            cell_configs.insert(
                BuildFileCell::new(cell_name),
                InterpreterCellInfo::new(
                    BuildFileCell::new(cell_name),
                    config,
                    cell_instance.cell_alias_resolver().dupe(),
                )?,
            );
        }
        Ok(Self {
            cell_resolver,
            cell_configs,
            build_file_global_env,
            extension_file_global_env,
            bxl_file_global_env,
            configuror: interpreter_configuror,
            disable_starlark_types,
        })
    }

    pub fn configuror(&self) -> &Arc<dyn InterpreterConfiguror> {
        &self.configuror
    }
}
