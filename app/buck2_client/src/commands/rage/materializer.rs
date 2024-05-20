/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_audit::deferred_materializer::DeferredMaterializerCommand;
use buck2_audit::deferred_materializer::DeferredMaterializerSubcommand;
use buck2_audit::AuditCommand;
use buck2_cli_proto::ClientContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_client_ctx::daemon::client::connect::BootstrapBuckdClient;
use buck2_client_ctx::events_ctx::PartialResultCtx;
use buck2_client_ctx::events_ctx::PartialResultHandler;
use buck2_client_ctx::subscribers::subscriber::EventSubscriber;
use buck2_client_ctx::subscribers::subscribers::EventSubscribers;
use buck2_common::manifold::ManifoldClient;
use futures::future::BoxFuture;
use futures::future::Shared;

use crate::commands::rage::manifold::buf_to_manifold;
use crate::commands::rage::MaterializerRageUploadData;

pub async fn upload_materializer_data(
    buckd: Shared<BoxFuture<'_, buck2_error::Result<BootstrapBuckdClient>>>,
    client_context: &ClientContext,
    manifold: &ManifoldClient,
    manifold_id: &String,
    materializer_data: MaterializerRageUploadData,
) -> anyhow::Result<String> {
    let mut buckd =
        buckd.await?.with_subscribers(EventSubscribers::new(
            vec![Box::new(TracingSubscriber) as _],
        ));

    let mut capture = CaptureStdout::new();

    let outcome = buckd
        .with_flushing()
        .audit(
            buck2_cli_proto::GenericRequest {
                context: Some(client_context.clone()),
                serialized_opts: serde_json::to_string(&AuditCommand::DeferredMaterializer(
                    DeferredMaterializerCommand {
                        common_opts: Default::default(),
                        _target_cfg: Default::default(),
                        subcommand: match materializer_data {
                            MaterializerRageUploadData::State => {
                                DeferredMaterializerSubcommand::List
                            }
                            MaterializerRageUploadData::Fsck => {
                                DeferredMaterializerSubcommand::Fsck
                            }
                        },
                    },
                ))?,
            },
            None,
            &mut capture,
        )
        .await?;

    match outcome {
        CommandOutcome::Success(..) => {}
        CommandOutcome::Failure(..) => return Err(anyhow::anyhow!("Command failed")),
    }

    let manifold_filename = format!("flat/{}_materializer_{}", manifold_id, materializer_data);
    buf_to_manifold(manifold, &capture.buf, manifold_filename).await
}

/// Receive StdoutBytes, just capture them.
struct CaptureStdout {
    buf: Vec<u8>,
}

impl CaptureStdout {
    fn new() -> Self {
        Self { buf: Vec::new() }
    }
}

#[async_trait]
impl PartialResultHandler for CaptureStdout {
    type PartialResult = buck2_cli_proto::StdoutBytes;

    async fn handle_partial_result(
        &mut self,
        _ctx: PartialResultCtx<'_, '_>,
        partial_res: Self::PartialResult,
    ) -> anyhow::Result<()> {
        self.buf.extend(partial_res.data);
        Ok(())
    }
}

struct TracingSubscriber;

#[async_trait]
impl EventSubscriber for TracingSubscriber {
    async fn handle_tailer_stderr(&mut self, stderr: &str) -> anyhow::Result<()> {
        tracing::info!("{}", stderr);
        Ok(())
    }

    async fn handle_error(&mut self, error: &buck2_error::Error) -> anyhow::Result<()> {
        tracing::info!("{:#}", error);
        Ok(())
    }

    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        tracing::info!("{:?}", result);
        Ok(())
    }
}
