/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::common::ConsoleType;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::protobuf_util::ProtobufSplitter;
use buck2_client_ctx::stream_util::reborrow_stream_for_static;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_subscription_proto::SubscriptionRequest;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use once_cell::sync::Lazy;
use prost::Message;
use tokio_util::codec::FramedRead;

/// Open a subscription channel to the Buck2 daemon. This allows you to interact with the Buck2
/// daemon via the `stdin` and `stdout` of this command: you send requests to the daemon by writing
/// to `stdin`, and you get responses via `stdout`.
///
/// The protocol used by this command is length-prefixed protobuf. This format is a repeated series
/// of a varint followed by a record of the length indicated by said varint.
///
/// The protobuf spec for those records is described in
/// `buck2_subscription_proto/subscription.proto`. The client writes `SubscriptionRequest` and
/// reads `SubscriptionResponse`. See the documentation in `subscription.proto` to discover
/// available APIs.
///
/// This API does not (currently) allow invalid requests and will error out when one is sent.
#[derive(Debug, clap::Parser)]
#[clap(about = "Subscribe to updates from the Buck2 daemon")]
pub struct SubscribeCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,
}

#[async_trait]
impl StreamingCommand for SubscribeCommand {
    const COMMAND_NAME: &'static str = "subscribe";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let client_context =
            ctx.client_context(&self.config_opts, matches, self.sanitized_argv())?;

        let stream = FramedRead::new(ctx.stdin(), ProtobufSplitter)
            .and_then(|bytes| {
                futures::future::ready(
                    SubscriptionRequest::decode_length_delimited(bytes)
                        .context("Error decoding SubscriptionRequest"),
                )
            })
            .map(|res| match res {
                Ok(r) => r,
                Err(e) => {
                    // NOTE: if stderr is gone there is not much we can do besides not write to
                    // stderr.
                    let _ignored = buck2_client_ctx::eprintln!("Error parsing request: {:#}", e);
                    SubscriptionRequest {
                        request: Some(buck2_subscription_proto::Disconnect {}.into()),
                    }
                }
            })
            .map(|request| buck2_cli_proto::SubscriptionRequestWrapper {
                request: Some(request),
            });

        reborrow_stream_for_static(
            stream,
            |stream| async move {
                buckd
                    .with_flushing()
                    .subscription(client_context, stream)
                    .await
            },
            || {
                Some(buck2_cli_proto::SubscriptionRequestWrapper {
                    request: Some(SubscriptionRequest {
                        request: Some(buck2_subscription_proto::Disconnect {}.into()),
                    }),
                })
            },
        )
        .await??;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        // This should only be communicated with by an IDE, so disable anything other
        // than the simple console
        static SIMPLE_CONSOLE: Lazy<CommonConsoleOptions> = Lazy::new(|| CommonConsoleOptions {
            console_type: ConsoleType::Simple,
            ui: vec![],
            no_interactive_console: true,
        });
        &SIMPLE_CONSOLE
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        &self.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.config_opts
    }
}
