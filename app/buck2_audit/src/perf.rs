/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Starlark debugging.

pub mod configured_graph_size;

use async_trait::async_trait;
use buck2_client_ctx::common::CommonCommandOptions;

use crate::AuditSubcommand;
use crate::perf::configured_graph_size::ConfiguredGraphSizeCommand;

#[derive(Debug, clap::Subcommand, serde::Serialize, serde::Deserialize)]
#[clap(name = "perf", about = "Commands for checking buck2 performance")]
pub enum AuditPerfCommand {
    ConfiguredGraphSize(ConfiguredGraphSizeCommand),
}

#[async_trait]
impl AuditSubcommand for AuditPerfCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        match self {
            AuditPerfCommand::ConfiguredGraphSize(cmd) => &cmd.common_opts,
        }
    }
}
