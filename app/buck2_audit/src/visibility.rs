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
    name = "audit-visibility",
    about = "Verify the visibility for transitive deps of the specified target(s) on the unconfigured target graph"
)]
pub struct AuditVisibilityCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    #[clap(name = "TARGET_PATTERNS", help = "Target pattern(s) to analyze.")]
    pub patterns: Vec<String>,
}

#[async_trait]
impl AuditSubcommand for AuditVisibilityCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
