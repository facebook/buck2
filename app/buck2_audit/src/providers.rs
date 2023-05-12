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
    name = "audit-providers",
    about = "prints out the providers for a target pattern"
)]
pub struct AuditProvidersCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    #[clap(name = "TARGET_PATTERNS", help = "Patterns to analyze")]
    pub patterns: Vec<String>,

    #[clap(long, conflicts_with_all=&["list", "print-debug"])]
    pub quiet: bool,

    #[clap(
        long,
        short = 'l',
        help = "List the available providers", conflicts_with_all=&["print-debug", "quiet"]
    )]
    pub list: bool,

    #[clap(
        long = "print-debug",
        help = "Print the providers using debug format (very verbose)",
        conflicts_with_all=&["list", "quiet"]
    )]
    pub print_debug: bool,
}

#[async_trait]
impl AuditSubcommand for AuditProvidersCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
