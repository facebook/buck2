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
    name = "audit-analysis-queries",
    about = "buck audit analysis resolving query attrs"
)]
pub struct AuditAnalysisQueriesCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    #[clap(
        name = "TARGET_PATTERNS",
        help = "Patterns to evaluate. The query attributes for targets matching these patterns will be evaluated"
    )]
    pub patterns: Vec<String>,

    #[clap(
        long,
        help = "Enable to print the outputs for the targets in the resolved queries"
    )]
    pub include_outputs: bool,
}

#[async_trait]
impl AuditSubcommand for AuditAnalysisQueriesCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
