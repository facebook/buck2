/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_cli_proto::trace_io_request;
use buck2_cli_proto::trace_io_response;
use buck2_common::file_ops::RawSymlink;
use buck2_common::io::trace::TracingIoProvider;
use buck2_events::dispatch::span_async;
use buck2_server_ctx::command_end::command_end;
use buck2_server_ctx::ctx::ServerCommandContextTrait;

use crate::ctx::ServerCommandContext;

pub(crate) async fn trace_io_command(
    context: ServerCommandContext,
    req: buck2_cli_proto::TraceIoRequest,
) -> anyhow::Result<buck2_cli_proto::TraceIoResponse> {
    let metadata = context.request_metadata().await?;
    let start_event = buck2_data::CommandStart {
        metadata: metadata.clone(),
        data: Some(buck2_data::TraceIoCommandStart {}.into()),
    };
    span_async(start_event, async move {
        let tracing_provider = &context
            .base_context
            .io
            .as_any()
            .downcast_ref::<TracingIoProvider>();
        let respond_with_trace = matches!(
            req.read_state,
            Some(trace_io_request::ReadIoTracingState { with_trace: true })
        );

        let result = match (tracing_provider, respond_with_trace) {
            (Some(provider), true) => build_response_with_trace(&context, provider).await,
            (Some(_), false) => Ok(buck2_cli_proto::TraceIoResponse {
                enabled: true,
                trace: Vec::new(),
                relative_symlinks: Vec::new(),
                external_symlinks: Vec::new(),
            }),
            (None, _) => Ok(buck2_cli_proto::TraceIoResponse {
                enabled: false,
                trace: Vec::new(),
                relative_symlinks: Vec::new(),
                external_symlinks: Vec::new(),
            }),
        };

        let end_event = command_end(metadata, &result, buck2_data::TraceIoCommandEnd {});
        (result, end_event)
    })
    .await
}

async fn build_response_with_trace(
    context: &ServerCommandContext,
    provider: &TracingIoProvider,
) -> anyhow::Result<buck2_cli_proto::TraceIoResponse> {
    // Materialize buck-out paths so they can be archived.
    let buck_out_paths: Vec<_> = provider
        .trace()
        .buck_out_entries
        .iter()
        .map(|path| path.key().to_buf())
        .collect();
    context
        .materializer()
        .ensure_materialized(buck_out_paths)
        .await
        .context("Error materializing buck-out paths for trace")?;

    let mut entries: Vec<_> = provider
        .trace()
        .entries
        .iter()
        .map(|path| path.to_string())
        .collect();
    entries.extend(
        provider
            .trace()
            .buck_out_entries
            .iter()
            .map(|path| path.to_string()),
    );

    let mut relative_symlinks = Vec::new();
    let mut external_symlinks = Vec::new();
    for link in provider.trace().symlinks.iter() {
        match &link.to {
            RawSymlink::Relative(to) => {
                relative_symlinks.push(trace_io_response::RelativeSymlink {
                    link: link.at.to_string(),
                    target: to.to_string(),
                });
            }
            RawSymlink::External(external) => {
                external_symlinks.push(trace_io_response::ExternalSymlink {
                    link: link.at.to_string(),
                    target: external.target_str().to_owned(),
                    remaining_path: external.remaining_path().map(|path| path.to_string()),
                });
            }
        }
    }

    Ok(buck2_cli_proto::TraceIoResponse {
        enabled: true,
        trace: entries,
        relative_symlinks,
        external_symlinks,
    })
}
