/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_client_ctx::common::CommonCommandOptions;

use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "audit-dep-files",
    about = "prints out the select files for a command"
)]
pub struct AuditDepFilesCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    #[clap(help = "Target to query dep files for")]
    pub pattern: String,

    #[clap(help = "Action category")]
    pub category: String,

    #[clap(help = "Action identifier")]
    pub identifier: Option<String>,
}

#[async_trait]
impl AuditSubcommand for AuditDepFilesCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
