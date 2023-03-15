/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use anyhow::Context;
use async_trait::async_trait;
use buck2_cli_proto::ClientContext;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use futures::stream::StreamExt;

use crate::AuditCommandCommonOptions;
use crate::AuditSubcommand;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "deferred-materializer",
    about = "Access and interact with the deferred materializer"
)]
pub struct DeferredMaterializerCommand {
    #[clap(flatten)]
    pub common_opts: AuditCommandCommonOptions,

    #[clap(subcommand)]
    pub subcommand: DeferredMaterializerSubcommand,
}

#[derive(Debug, clap::Subcommand, serde::Serialize, serde::Deserialize)]
pub enum DeferredMaterializerSubcommand {
    List,
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
    async fn server_execute(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let mut stdout = stdout.as_writer();

        let materializer = server_ctx.materializer();
        let deferred_materializer = materializer
            .as_deferred_materializer_extension()
            .context("Deferred materializer is not in use")?;

        match self.subcommand {
            DeferredMaterializerSubcommand::List => {
                let mut stream = deferred_materializer
                    .iterate()
                    .context("Failed to start iterating")?;

                while let Some((path, entry)) = stream.next().await {
                    writeln!(stdout, "{}\t{}", path, entry)?;
                }
            }
            DeferredMaterializerSubcommand::Refresh { min_ttl } => {
                deferred_materializer
                    .refresh_ttls(min_ttl)
                    .await
                    .context("Failed to refresh")?;
            }
            DeferredMaterializerSubcommand::GetRefreshLog => {
                let text = deferred_materializer
                    .get_ttl_refresh_log()
                    .await
                    .context("Failed to get_ttl_refresh_log")?;

                write!(stdout, "{}", text)?;
            }
            DeferredMaterializerSubcommand::TestIter { count } => {
                let text = deferred_materializer
                    .test_iter(count)
                    .await
                    .context("Failed to test_iter")?;

                write!(stdout, "{}", text)?;
            }
        }

        anyhow::Ok(())
    }

    fn common_opts(&self) -> &AuditCommandCommonOptions {
        &self.common_opts
    }
}
