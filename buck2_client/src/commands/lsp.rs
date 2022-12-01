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
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_client_ctx::LSP_COMMAND_NAME;
use bytes::BytesMut;
use cli_proto::LspRequest;
use futures::future::Either;
use futures::future::Future;
use futures::stream::Stream;
use futures::stream::StreamExt;
use lsp_server::Message;
use once_cell::sync::Lazy;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
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
    const COMMAND_NAME: &'static str = LSP_COMMAND_NAME;

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let client_context =
            ctx.client_context(&self.config_opts, matches, self.sanitized_argv())?;

        let stream = FramedRead::new(ctx.stdin(), LspMessageDecoder).filter_map(|m| {
            let m = m.and_then(|m| {
                let lsp_json = serde_json::to_string(&m)?;
                Ok(LspRequest { lsp_json })
            });

            futures::future::ready(match m {
                Ok(m) => Some(m),
                Err(e) => {
                    let _ignored =
                        buck2_client_ctx::eprintln!("Could not read message from stdin: `{}`", e);
                    None
                }
            })
        });

        reborrow_stream_for_static(stream, |stream| async move {
            buckd.with_flushing().lsp(client_context, stream).await
        })
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

struct LspMessageDecoder;

impl Decoder for LspMessageDecoder {
    type Item = Message;
    type Error = anyhow::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        // The LSP protocol allows at most 2 headers (Content-Length and Content-Type), but since a
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

/// We need to provide a 'static stream for Tonic to send to the Buck2 daemon, but we don't want to
/// borrow stdin statically (though in practice that doesn't really matter because the way the
/// command ends is when stdin is empty). So, what we do instead is that we forward stdin only
/// while the command is ongoing.
async fn reborrow_stream_for_static<'a, T, R, F>(
    stream: impl Stream<Item = T> + 'a,
    f: impl FnOnce(ReceiverStream<T>) -> F,
) -> R
where
    F: Future<Output = R> + 'a,
    T: 'static,
{
    let (tx, rx) = mpsc::channel(1);

    let forward = async move {
        futures::pin_mut!(stream);

        while let Ok(permit) = tx.reserve().await {
            match stream.next().await {
                Some(e) => permit.send(e),
                None => break,
            }
        }

        // The LSP server side does not handle hangups. So, until it does... we never hang up:
        // Err(Status { code: FailedPrecondition, message: "received a message that is not a `StreamingRequest`", source: None })
        let out = futures::future::pending().await;

        // We actually can't get here; Note that we actually get his function to return a "R"
        // because pending() can return whatever we want it to, but that's bcause it can't actually
        // return :)
        drop(tx);

        out
    };

    let rx = ReceiverStream::new(rx);
    let work = f(rx);

    futures::pin_mut!(work);
    futures::pin_mut!(forward);

    Either::factor_first(futures::future::select(work, forward).await).0
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

        // Decoding a subset should return None and not consume anything.
        {
            let mut tmp = BytesMut::new();
            tmp.extend_from_slice(&bytes[0..8]);
            assert_matches!(LspMessageDecoder.decode(&mut tmp)?, None);
            assert_eq!(tmp.len(), 8);
        }

        assert_matches!(LspMessageDecoder.decode(&mut bytes)?, Some(Message::Request(Request {
            id, method, params
        })) => {
            assert_eq!(id, r1.id);
            assert_eq!(method, r1.method);
            assert_eq!(params, r1.params);
        });

        assert_matches!(LspMessageDecoder.decode(&mut bytes)?, Some(Message::Request(Request {
            id, method, params
        })) => {
            assert_eq!(id, r2.id);
            assert_eq!(method, r2.method);
            assert_eq!(params, r2.params);
        });

        assert_matches!(LspMessageDecoder.decode(&mut bytes)?, None);
        assert_eq!(bytes.len(), 0);

        Ok(())
    }
}
