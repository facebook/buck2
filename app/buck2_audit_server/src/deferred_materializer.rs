/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use async_trait::async_trait;
use buck2_audit::deferred_materializer::DeferredMaterializerCommand;
use buck2_audit::deferred_materializer::DeferredMaterializerSubcommand;
use buck2_cli_proto::ClientContext;
use buck2_error::BuckErrorContext;
use buck2_execute::materialize::materializer::DeferredMaterializerIterItem;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use futures::stream::StreamExt;

use crate::ServerAuditSubcommand;

#[async_trait]
impl ServerAuditSubcommand for DeferredMaterializerCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> buck2_error::Result<()> {
        let mut stdout = stdout.as_writer();

        let materializer = server_ctx.materializer();
        let deferred_materializer = materializer
            .as_deferred_materializer_extension()
            .buck_error_context("Deferred materializer is not in use")?;

        match self.subcommand {
            DeferredMaterializerSubcommand::List => {
                let mut stream = deferred_materializer
                    .iterate()
                    .buck_error_context("Failed to start iterating")?;

                while let Some(DeferredMaterializerIterItem {
                    artifact_path,
                    artifact_display,
                    deps,
                }) = stream.next().await
                {
                    writeln!(stdout, "{artifact_path}\t{artifact_display}")?;
                    writeln!(stdout, "  deps: {}", deps.len())?;
                    for (dep_path, dep_kind) in deps {
                        writeln!(stdout, "    {dep_path} {dep_kind}")?;
                    }
                }
            }
            DeferredMaterializerSubcommand::ListSubscriptions => {
                let mut stream = deferred_materializer
                    .list_subscriptions()
                    .buck_error_context("Failed to start listing subscriptions")?;

                while let Some(path) = stream.next().await {
                    writeln!(stdout, "{}", path)?;
                }
            }
            DeferredMaterializerSubcommand::Fsck => {
                let mut stream = deferred_materializer
                    .fsck()
                    .buck_error_context("Failed to start iterating")?;

                let mut n = 0;

                while let Some((path, error)) = stream.next().await {
                    n += 1;
                    writeln!(stdout, "{}\t{:#}", path, error)?;
                }

                let mut stderr = server_ctx.stderr()?;
                writeln!(&mut stderr, "total errors: {}", n)?;
            }
            DeferredMaterializerSubcommand::Refresh { min_ttl } => {
                deferred_materializer
                    .refresh_ttls(min_ttl)
                    .await
                    .buck_error_context("Failed to refresh")?;
            }
            DeferredMaterializerSubcommand::GetRefreshLog => {
                let text = deferred_materializer
                    .get_ttl_refresh_log()
                    .await
                    .buck_error_context("Failed to get_ttl_refresh_log")?;

                write!(stdout, "{}", text)?;
            }
            DeferredMaterializerSubcommand::TestIter { count } => {
                let text = deferred_materializer
                    .test_iter(count)
                    .await
                    .buck_error_context("Failed to test_iter")?;

                write!(stdout, "{}", text)?;
            }
            DeferredMaterializerSubcommand::FlushAccessTimes => {
                let text = deferred_materializer
                    .flush_all_access_times()
                    .await
                    .buck_error_context("Failed to flush all access times")?;

                write!(stdout, "{}", text)?;
            }
        }

        buck2_error::Ok(())
    }
}
