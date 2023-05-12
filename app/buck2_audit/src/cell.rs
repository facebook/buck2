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
    name = "audit-cell",
    about = "Query information about the [repositories] list in .buckconfig."
)]
pub struct AuditCellCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    #[clap(long = "json", help = "Output in JSON format")]
    pub json: bool,

    #[clap(
        long = "paths-only",
        help = "Don't include the cell name in the output"
    )]
    pub paths_only: bool,

    #[clap(
        long = "aliases",
        help = "If enabled and no explicit aliases are passed, will query for all aliases in the working directory cell."
    )]
    pub aliases: bool,

    #[clap(
        name = "CELL_ALIASES",
        help = "Cell aliases to query. These aliases will be resolved in the working directory cell."
    )]
    pub aliases_to_resolve: Vec<String>,
}

#[async_trait]
impl AuditSubcommand for AuditCellCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
