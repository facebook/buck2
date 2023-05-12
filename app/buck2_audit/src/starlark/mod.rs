/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Starlark debugging.

pub mod module;
pub mod package_deps;

use async_trait::async_trait;
use buck2_client_ctx::common::CommonCommandOptions;

use crate::starlark::module::StarlarkModuleCommand;
use crate::starlark::package_deps::StarlarkPackageDepsCommand;
use crate::AuditSubcommand;

#[derive(Debug, clap::Subcommand, serde::Serialize, serde::Deserialize)]
#[clap(name = "starlark", about = "Debug Starlark interpreter")]
pub enum StarlarkCommand {
    Module(StarlarkModuleCommand),
    PackageDeps(StarlarkPackageDepsCommand),
}

#[async_trait]
impl AuditSubcommand for StarlarkCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        match self {
            StarlarkCommand::Module(cmd) => &cmd.common_opts,
            StarlarkCommand::PackageDeps(cmd) => &cmd.common_opts,
        }
    }
}
