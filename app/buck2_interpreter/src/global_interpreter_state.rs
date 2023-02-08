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
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::view::LegacyBuckConfigsView;
use buck2_common::result::SharedResult;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::CellResolver;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use starlark::environment::Globals;

use crate::dice::starlark_types::GetDisableStarlarkTypes;
use crate::dice::HasInterpreterContext;
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

#[async_trait]
pub trait HasGlobalInterpreterState {
    async fn get_global_interpreter_state(&self) -> SharedResult<Arc<GlobalInterpreterState>>;
}

#[async_trait]
impl HasGlobalInterpreterState for DiceComputations {
    async fn get_global_interpreter_state(&self) -> SharedResult<Arc<GlobalInterpreterState>> {
        #[derive(Clone, Dupe, Allocative)]
        struct GisValue(Arc<GlobalInterpreterState>);

        #[derive(
            Clone,
            derive_more::Display,
            Dupe,
            Debug,
            Eq,
            Hash,
            PartialEq,
            Allocative
        )]
        #[display(fmt = "{:?}", self)]
        struct GisKey();

        #[async_trait]
        impl Key for GisKey {
            type Value = SharedResult<GisValue>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let interpreter_configuror = ctx.get_interpreter_configuror().await?;
                let legacy_configs = ctx.get_legacy_configs_on_dice().await?;
                let cell_resolver = ctx.get_cell_resolver().await?;
                let disable_starlark_types = ctx.get_disable_starlark_types().await?;

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

        Ok(self.compute(&GisKey()).await??.0)
    }
}
