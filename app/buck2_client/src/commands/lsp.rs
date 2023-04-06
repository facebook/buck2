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
use buck2_cli_proto::LspRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::common::ConsoleType;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::stream_util::reborrow_stream_for_static;
use buck2_client_ctx::streaming::StreamingCommand;
use bytes::BytesMut;
use futures::stream::StreamExt;
use futures::Stream;
use lsp_server::Message;
use once_cell::sync::Lazy;
use serde::Deserialize;
use serde::Serialize;
use tokio::io::AsyncRead;
use tokio_util::codec::Decoder;
use tokio_util::codec::FramedRead;

#[derive(Debug, clap::Parser)]
#[clap(about = "Start an LSP server for starlark files")]
pub struct LspCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,
}

#[async_trait]
impl StreamingCommand for LspCommand {
    const COMMAND_NAME: &'static str = "lsp";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let client_context =
            ctx.client_context(&self.config_opts, matches, self.sanitized_argv())?;
        let stream = ide_message_stream::<_, Message>(ctx.stdin()).filter_map(|m| async move {
            match m {
                Ok(lsp_json) => Some(LspRequest { lsp_json }),
                Err(e) => {
                    let _ignored =
                        buck2_client_ctx::eprintln!("Could not read message from stdin: `{}`", e);
                    None
                }
            }
        });

        reborrow_stream_for_static(
            stream,
            |stream| async move { buckd.with_flushing().lsp(client_context, stream).await },
            // The LSP server side does not handle hangups. So, until it does... we never hang up:
            // Err(Status { code: FailedPrecondition, message: "received a message that is not a `StreamingRequest`", source: None })
            || None,
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

    fn should_show_waiting_message(&self) -> bool {
        // If we're running the LSP, do not show "Waiting for daemon..." if we do not get any spans.
        false
    }
}
/// Reads from input a stream of lsp-like messages and returns them as serde_json serialized strings.
pub fn ide_message_stream<T: AsyncRead, Message: for<'a> Deserialize<'a> + Serialize>(
    input: T,
) -> impl Stream<Item = anyhow::Result<String>> {
    FramedRead::new(
        input,
        LspMessageLikeDecoder::<Message> {
            _marker: std::marker::PhantomData,
        },
    )
    .map(|m| m.and_then(|m| Ok(serde_json::to_string(&m)?)))
}

pub struct LspMessageLikeDecoder<T: for<'a> Deserialize<'a>> {
    _marker: std::marker::PhantomData<T>,
}

impl<T: for<'a> Deserialize<'a>> Decoder for LspMessageLikeDecoder<T> {
    type Item = T;
    type Error = anyhow::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        // The LSP (and DAP) protocol allows at most 2 headers (Content-Length and Content-Type), but since a
        // header is 2 pointers we allow ourselves quite a few more.
        let mut headers_buff = [httparse::EMPTY_HEADER; 16];

        let (headers_length, headers) =
            match httparse::parse_headers(src, &mut headers_buff).context("Invalid headers")? {
                httparse::Status::Complete(r) => r,
                httparse::Status::Partial => return Ok(None),
            };

        let mut content_length: Option<usize> = None;

        for h in headers {
            if h.name.eq_ignore_ascii_case("Content-Length") {
                content_length = Some(
                    std::str::from_utf8(h.value)
                        .context("Content-Length is not utf-8")?
                        .parse()
                        .context("Content-Length is not a number")?,
                );
                break;
            }
        }

        let content_length = content_length.context("Content-Length is missing")?;

        if src.len() < headers_length + content_length {
            return Ok(None);
        }

        let _headers = src.split_to(headers_length);
        let text = src.split_to(content_length);
        Some(serde_json::from_slice(&text).context("Invalid request")).transpose()
    }
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use lsp_server::Message;
    use lsp_server::Request;
    use lsp_server::RequestId;

    use super::*;

    #[test]
    fn test_decoder() -> anyhow::Result<()> {
        let r1 = Request {
            id: RequestId::from(1i32),
            method: "m1".into(),
            params: "foobar".into(),
        };

        let r2 = Request {
            id: RequestId::from(2i32),
            method: "m2".into(),
            params: "barbaz".into(),
        };

        let mut bytes = BytesMut::new();

        {
            // This wants a `Write` and BytesMut doesn't do that so just use a Vec.
            let mut tmp = Vec::new();
            Message::Request(r1.clone()).write(&mut tmp)?;
            Message::Request(r2.clone()).write(&mut tmp)?;
            bytes.extend_from_slice(&tmp);
        };

        let mut decoder = LspMessageLikeDecoder {
            _marker: std::marker::PhantomData,
        };

        // Decoding a subset should return None and not consume anything.
        {
            let mut tmp = BytesMut::new();
            tmp.extend_from_slice(&bytes[0..8]);
            assert_matches!(decoder.decode(&mut tmp)?, None);
            assert_eq!(tmp.len(), 8);
        }

        assert_matches!(decoder.decode(&mut bytes)?, Some(Message::Request(Request {
            id, method, params
        })) => {
            assert_eq!(id, r1.id);
            assert_eq!(method, r1.method);
            assert_eq!(params, r1.params);
        });

        assert_matches!(decoder.decode(&mut bytes)?, Some(Message::Request(Request {
            id, method, params
        })) => {
            assert_eq!(id, r2.id);
            assert_eq!(method, r2.method);
            assert_eq!(params, r2.params);
        });

        assert_matches!(decoder.decode(&mut bytes)?, None);
        assert_eq!(bytes.len(), 0);

        Ok(())
    }
}
