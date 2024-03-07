/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::cells::CellResolver;
use buck2_futures::cancellation::CancellationContext;
use buck2_interpreter::dice::starlark_types::GetStarlarkTypes;
use buck2_interpreter::file_type::StarlarkFileType;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use starlark::environment::Globals;

use crate::interpreter::configuror::BuildInterpreterConfiguror;
use crate::interpreter::context::HasInterpreterContext;

/// Information shared across interpreters. Contains no cell-specific
/// information.
#[derive(Allocative)]
pub struct GlobalInterpreterState {
    pub cell_resolver: CellResolver,

    /// The GlobalEnvironment contains all the globally available symbols
    /// (primarily starlark stdlib and Buck-provided functions) that should
    /// be available in a build file.
    pub build_file_global_env: Globals,

    /// Symbols for `PACKAGE` files.
    pub package_file_global_env: Globals,

    /// The GlobalEnvironment contains all the globally available symbols
    /// (primarily starlark stdlib and Buck-provided functions) that should
    /// be available in an extension file.
    pub extension_file_global_env: Globals,

    /// The GlobalEnvironment contains all the globally available symbols
    /// (primarily starlark stdlib and Buck-provided functions) that should
    /// be available in a bxl file.
    pub bxl_file_global_env: Globals,

    /// Interpreter Configurer
    pub configuror: Arc<BuildInterpreterConfiguror>,

    /// Check types in Starlark (or just parse and ignore).
    pub disable_starlark_types: bool,

    /// Static typechecking for bzl and bxl files.
    pub unstable_typecheck: bool,
}

impl GlobalInterpreterState {
    pub fn new(
        cell_resolver: CellResolver,
        interpreter_configuror: Arc<BuildInterpreterConfiguror>,
        disable_starlark_types: bool,
        unstable_typecheck: bool,
    ) -> anyhow::Result<Self> {
        // TODO: There should be one of these that also does not have native functions
        // in the global       namespace so that it can be configured per-cell
        let build_file_global_env = interpreter_configuror.build_file_globals();
        let package_file_global_env = interpreter_configuror.package_file_globals();
        let extension_file_global_env = interpreter_configuror.extension_file_globals();
        let bxl_file_global_env = interpreter_configuror.bxl_file_globals();

        Ok(Self {
            cell_resolver,
            build_file_global_env,
            package_file_global_env,
            extension_file_global_env,
            bxl_file_global_env,
            configuror: interpreter_configuror,
            disable_starlark_types,
            unstable_typecheck,
        })
    }

    pub fn configuror(&self) -> &Arc<BuildInterpreterConfiguror> {
        &self.configuror
    }

    pub fn globals_for_file_type(&self, file_type: StarlarkFileType) -> &Globals {
        match file_type {
            StarlarkFileType::Buck => &self.build_file_global_env,
            StarlarkFileType::Package => &self.package_file_global_env,
            StarlarkFileType::Bzl => &self.extension_file_global_env,
            StarlarkFileType::Bxl => &self.bxl_file_global_env,
        }
    }
}

#[async_trait]
pub trait HasGlobalInterpreterState {
    async fn get_global_interpreter_state(&mut self)
    -> anyhow::Result<Arc<GlobalInterpreterState>>;
}

#[async_trait]
impl HasGlobalInterpreterState for DiceComputations<'_> {
    async fn get_global_interpreter_state(
        &mut self,
    ) -> anyhow::Result<Arc<GlobalInterpreterState>> {
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
            type Value = buck2_error::Result<GisValue>;
            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                let interpreter_configuror = ctx.get_interpreter_configuror().await?;
                let cell_resolver = ctx.get_cell_resolver().await?;
                let disable_starlark_types = ctx.get_disable_starlark_types().await?;
                let unstable_typecheck = ctx.get_unstable_typecheck().await?;

                Ok(GisValue(Arc::new(GlobalInterpreterState::new(
                    cell_resolver,
                    interpreter_configuror,
                    disable_starlark_types,
                    unstable_typecheck,
                )?)))
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                false
            }
        }

        Ok(self.compute(&GisKey()).await??.0)
    }
}
