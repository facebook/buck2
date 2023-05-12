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
    name = "audit-classpath",
    about = "Prints out a target's classpaths if it has one.
    This command is deprecated and currently available for compatibility with buck1.
    We will replace this command with something that can audit the entire `TemplatePlaceholderInfo` in the future."
)]
pub struct AuditClasspathCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    #[clap(name = "TARGET_PATTERNS", help = "Target patterns to audit")]
    pub patterns: Vec<String>,

    /// Output in JSON format
    #[clap(long)]
    pub json: bool,
    // TODO(scottcao): Add --show-targets, --dot, and other relevant flags
}

#[async_trait]
impl AuditSubcommand for AuditClasspathCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
