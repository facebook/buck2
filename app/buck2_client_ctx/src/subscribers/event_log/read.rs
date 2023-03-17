/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::pin::Pin;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::task::Context;
use std::task::Poll;
use std::time::SystemTime;

use anyhow::Context as _;
use async_compression::tokio::bufread::GzipDecoder;
use async_compression::tokio::bufread::ZstdDecoder;
use buck2_cli_proto::*;
use buck2_core::fs::async_fs_util;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_events::trace::TraceId;
use buck2_events::BuckEvent;
use futures::stream::BoxStream;
use futures::stream::Stream;
use futures::stream::TryStreamExt;
use futures::StreamExt;
use pin_project::pin_project;
use prost::Message;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncRead;
use tokio::io::BufReader;
use tokio::io::ReadBuf;
use tokio_stream::wrappers::LinesStream;
use tokio_util::codec::FramedRead;

use crate::protobuf_util::ProtobufSplitter;
use crate::stream_value::StreamValue;
use crate::subscribers::event_log::utils::Compression;
use crate::subscribers::event_log::utils::Encoding;
use crate::subscribers::event_log::utils::EventLogErrors;
use crate::subscribers::event_log::utils::EventLogInferenceError;
use crate::subscribers::event_log::utils::Invocation;
use crate::subscribers::event_log::utils::LogMode;
use crate::subscribers::event_log::utils::NoInference;
use crate::subscribers::event_log::utils::KNOWN_ENCODINGS;

type EventLogReader<'a> = Box<dyn AsyncRead + Send + Sync + Unpin + 'a>;

pub struct ReaderStats {
    compressed_bytes: AtomicUsize,
    decompressed_bytes: AtomicUsize,
}

impl ReaderStats {
    pub fn new() -> Self {
        Self {
            compressed_bytes: AtomicUsize::new(0),
            decompressed_bytes: AtomicUsize::new(0),
        }
    }

    pub fn compressed_bytes(&self) -> usize {
        self.compressed_bytes.load(Ordering::Relaxed)
    }

    pub fn decompressed_bytes(&self) -> usize {
        self.decompressed_bytes.load(Ordering::Relaxed)
    }
}

mod counting_reader {
    #![allow(clippy::ref_option_ref)] // for the projection

    use super::*;

    #[pin_project]
    pub struct CountingReader<'a, T> {
        #[pin]
        pub(super) inner: T,
        pub(super) stats: Option<&'a AtomicUsize>,
    }
}

use counting_reader::CountingReader;

impl<'a, T> CountingReader<'a, T> {
    fn new(inner: T, stats: Option<&'a AtomicUsize>) -> Self {
        Self { inner, stats }
    }
}

impl<'a, T> AsyncRead for CountingReader<'a, T>
where
    T: AsyncRead,
{
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        let before = buf.remaining();

        let this = self.project();
        futures::ready!(this.inner.poll_read(cx, buf))?;
        if let Some(stats) = this.stats.as_mut() {
            stats.fetch_add(before - buf.remaining(), Ordering::Relaxed);
        }

        Poll::Ready(Ok(()))
    }
}

#[derive(Clone)]
pub struct EventLogPathBuf {
    pub(crate) path: AbsPathBuf,
    pub(crate) encoding: Encoding,
}

pub struct EventLogSummary {
    pub trace_id: TraceId,
    pub timestamp: SystemTime,
    pub invocation: Invocation,
}

impl EventLogPathBuf {
    pub fn infer(path: AbsPathBuf) -> anyhow::Result<Self> {
        Self::infer_opt(path)?
            .map_err(|NoInference(path)| EventLogInferenceError::InvalidExtension(path).into())
    }

    pub fn path(&self) -> &AbsPath {
        &self.path
    }

    pub(crate) fn infer_opt(path: AbsPathBuf) -> anyhow::Result<Result<Self, NoInference>> {
        let name = path
            .file_name()
            .with_context(|| EventLogInferenceError::NoFilename(path.clone()))?
            .to_str()
            .with_context(|| EventLogInferenceError::InvalidFilename(path.clone()))?;

        for encoding in KNOWN_ENCODINGS {
            for extension in encoding.extensions {
                if name.ends_with(extension) {
                    return Ok(Ok(Self {
                        path,
                        encoding: *encoding,
                    }));
                }
            }
        }

        Ok(Err(NoInference(path)))
    }

    async fn unpack_stream_json<'a>(
        &self,
        stats: Option<&'a ReaderStats>,
    ) -> anyhow::Result<(Invocation, BoxStream<'a, anyhow::Result<StreamValue>>)> {
        assert_eq!(self.encoding.mode, LogMode::Json);

        let log_file = self.open(stats).await?;
        let log_file = BufReader::new(log_file);
        let mut log_lines = log_file.lines();

        // This one is not an event.
        let header = log_lines
            .next_line()
            .await
            .context("Error reading header line")?
            .context("No header line")?;
        let invocation = serde_json::from_str::<Invocation>(&header)
            .with_context(|| format!("Invalid header: {}", header.trim_end()))?;

        let events = LinesStream::new(log_lines).map(|line| {
            let line = line.context("Error reading next line")?;
            serde_json::from_str::<StreamValue>(&line)
                .with_context(|| format!("Invalid line: {}", line.trim_end()))
        });

        Ok((invocation, events.boxed()))
    }

    async fn unpack_stream_protobuf<'a>(
        &self,
        stats: Option<&'a ReaderStats>,
    ) -> anyhow::Result<(Invocation, BoxStream<'a, anyhow::Result<StreamValue>>)> {
        assert_eq!(self.encoding.mode, LogMode::Protobuf);

        let log_file = self.open(stats).await?;
        let mut stream = FramedRead::new(log_file, ProtobufSplitter);

        let invocation = stream.try_next().await?.context("No invocation found")?;
        let invocation = buck2_data::Invocation::decode_length_delimited(invocation)
            .context("Invalid Invocation")?;
        let invocation = Invocation {
            command_line_args: invocation.command_line_args,
            working_dir: invocation.working_dir,
            trace_id: invocation
                .trace_id
                .map(|t| t.parse())
                .transpose()
                .context("Invalid TraceId")?
                .unwrap_or_else(TraceId::null),
        };

        let events = stream.and_then(|data| async move {
            let val = buck2_cli_proto::CommandProgress::decode_length_delimited(data)
                .context("Invalid CommandProgress")?;
            match val.progress {
                Some(command_progress::Progress::Event(event)) => Ok(StreamValue::Event(event)),
                Some(command_progress::Progress::Result(result)) => Ok(StreamValue::Result(result)),
                Some(command_progress::Progress::PartialResult(result)) => {
                    Ok(StreamValue::PartialResult(result))
                }
                None => Err(anyhow::anyhow!("Event type not recognized")),
            }
        });

        Ok((invocation, events.boxed()))
    }

    async fn unpack_stream_inner<'a>(
        &self,
        stats: Option<&'a ReaderStats>,
    ) -> anyhow::Result<(
        Invocation,
        impl Stream<Item = anyhow::Result<StreamValue>> + 'a,
    )> {
        match self.encoding.mode {
            LogMode::Json => self.unpack_stream_json(stats).await,
            LogMode::Protobuf => self.unpack_stream_protobuf(stats).await,
        }
    }

    /// Read the invocation line then the event stream.
    pub async fn unpack_stream_with_stats<'a>(
        &self,
        stats: &'a ReaderStats,
    ) -> anyhow::Result<(
        Invocation,
        impl Stream<Item = anyhow::Result<StreamValue>> + 'a,
    )> {
        self.unpack_stream_inner(Some(stats)).await
    }

    pub async fn unpack_stream(
        &self,
    ) -> anyhow::Result<(
        Invocation,
        impl Stream<Item = anyhow::Result<StreamValue>> + 'static,
    )> {
        self.unpack_stream_inner(None).await
    }

    async fn open<'a>(&self, stats: Option<&'a ReaderStats>) -> anyhow::Result<EventLogReader<'a>> {
        tracing::info!(
            "Open {} using encoding {:?}",
            self.path.display(),
            self.encoding
        );

        let (compressed_bytes, decompressed_bytes) = match stats {
            Some(stats) => (
                Some(&stats.compressed_bytes),
                Some(&stats.decompressed_bytes),
            ),
            None => (None, None),
        };

        let file = async_fs_util::open(&self.path).await?;
        let file = CountingReader::new(file, compressed_bytes);
        let file = match self.encoding.compression {
            Compression::None => {
                Box::new(CountingReader::new(file, decompressed_bytes)) as EventLogReader
            }
            Compression::Gzip => Box::new(CountingReader::new(
                GzipDecoder::new(BufReader::new(file)),
                decompressed_bytes,
            )) as EventLogReader,
            Compression::Zstd => Box::new(CountingReader::new(
                ZstdDecoder::new(BufReader::new(file)),
                decompressed_bytes,
            )) as EventLogReader,
        };

        Ok(file)
    }

    pub async fn get_summary(&self) -> anyhow::Result<EventLogSummary> {
        let (invocation, events) = self.unpack_stream().await?;
        let buck_event: BuckEvent = events
            .try_filter_map(|log| {
                let maybe_buck_event = match log {
                    StreamValue::Result(_) | StreamValue::PartialResult(_) => None,
                    StreamValue::Event(buck_event) => Some(buck_event),
                };
                futures::future::ready(Ok(maybe_buck_event))
            })
            .try_next()
            .await?
            .ok_or_else(|| {
                anyhow::anyhow!(EventLogErrors::EndOfFile(
                    self.path.to_str().unwrap().to_owned()
                ))
            })?
            .try_into()?;
        Ok(EventLogSummary {
            trace_id: buck_event.trace_id()?,
            timestamp: buck_event.timestamp(),
            invocation,
        })
    }

    pub fn extension(&self) -> &str {
        self.encoding.extensions[0]
    }
}
