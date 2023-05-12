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
    name = "deferred-materializer",
    about = "Access and interact with the deferred materializer"
)]
pub struct DeferredMaterializerCommand {
    #[clap(flatten)]
    pub common_opts: CommonCommandOptions,

    #[clap(subcommand)]
    pub subcommand: DeferredMaterializerSubcommand,
}

#[derive(Debug, clap::Subcommand, serde::Serialize, serde::Deserialize)]
pub enum DeferredMaterializerSubcommand {
    List,
    Fsck,
    Refresh {
        /// Minimum TTL to require for actions.
        #[clap()]
        min_ttl: i64,
    },
    /// Get the log for TTL refreshes.
    GetRefreshLog,
    TestIter {
        #[clap(long, default_value = "1")]
        count: usize,
    },
}

#[async_trait]
impl AuditSubcommand for DeferredMaterializerCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
