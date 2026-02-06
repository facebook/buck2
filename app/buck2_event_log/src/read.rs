/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::io;
use std::pin::Pin;
use std::str::FromStr;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::task::Context;
use std::task::Poll;
use std::time::SystemTime;

use async_compression::tokio::bufread::GzipDecoder;
use async_compression::tokio::bufread::ZstdDecoder;
use buck2_cli_proto::protobuf_util::ProtobufSplitter;
use buck2_cli_proto::*;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_events::BuckEvent;
use buck2_fs::async_fs_util;
use buck2_fs::error::IoResultExt;
use buck2_fs::paths::abs_path::AbsPath;
use buck2_fs::paths::abs_path::AbsPathBuf;
use buck2_wrapper_common::invocation_id::TraceId;
use futures::StreamExt;
use futures::stream::BoxStream;
use futures::stream::Stream;
use futures::stream::TryStreamExt;
use pin_project::pin_project;
use prost::Message;
use regex::Regex;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncRead;
use tokio::io::BufReader;
use tokio::io::ReadBuf;
use tokio_stream::wrappers::LinesStream;
use tokio_util::codec::FramedRead;

use crate::stream_value::StreamValue;
use crate::utils::Compression;
use crate::utils::Encoding;
use crate::utils::EventLogErrors;
use crate::utils::EventLogInferenceError;
use crate::utils::Invocation;
use crate::utils::KNOWN_ENCODINGS;
use crate::utils::LogMode;

type EventLogReader<'a> = Box<dyn AsyncRead + Send + Sync + Unpin + 'a>;

/// Check if an error is caused by a truncated compressed stream. This
/// can happen when reading in-progress event logs that don't have a
/// proper compression footer yet.
fn is_truncated_stream_error(error: &buck2_error::Error) -> bool {
    // Match exact error messages from compression-codecs library:
    // - zstd/bzip2/lz4: "... stream did not finish"
    // - gzip: "unexpected end of file" (default UnexpectedEof message)
    let msg = error.to_string().to_lowercase();
    msg.contains("did not finish") || msg.contains("unexpected end of file")
}

/// Wrap a stream to treat truncated compressed stream errors as EOF.
/// This allows reading in-progress event logs that may not have a
/// proper compression footer yet.
fn tolerant_of_truncation<T>(
    stream: impl Stream<Item = buck2_error::Result<T>>,
) -> impl Stream<Item = buck2_error::Result<T>> {
    stream.scan(false, |stopped, result| {
        if *stopped {
            return futures::future::ready(None);
        }
        match result {
            Ok(item) => futures::future::ready(Some(Ok(item))),
            Err(e) if is_truncated_stream_error(&e) => {
                // Treat truncation error as end of stream
                *stopped = true;
                futures::future::ready(None)
            }
            Err(e) => futures::future::ready(Some(Err(e))),
        }
    })
}

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

impl<T> AsyncRead for CountingReader<'_, T>
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
    pub fn infer(path: AbsPathBuf) -> buck2_error::Result<Self> {
        match Self::infer_opt(&path)? {
            Some(v) => Ok(v),
            None => Err(EventLogInferenceError::InvalidExtension(path).into()),
        }
    }

    pub fn path(&self) -> &AbsPath {
        &self.path
    }

    fn file_name(path: &AbsPathBuf) -> buck2_error::Result<&str> {
        let name = path
            .file_name()
            .ok_or_else(|| EventLogInferenceError::NoFilename(path.clone()))?
            .to_str()
            .ok_or_else(|| EventLogInferenceError::InvalidFilename(path.clone()))?;
        Ok(name)
    }

    pub fn uuid_from_filename(&self) -> buck2_error::Result<TraceId> {
        let name = Self::file_name(&self.path)?;
        let re = Regex::new(
            r"([a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12})",
        )?;
        let uuid = re
            .find(name)
            .ok_or(EventLogInferenceError::NoUuidInFilename(self.path.clone()))?
            .as_str();
        TraceId::from_str(uuid).buck_error_context("Failed to create TraceId from uuid")
    }

    // TODO iguridi: this should be done by parsing file header
    pub fn command_from_filename(&self) -> buck2_error::Result<&str> {
        let file_name = Self::file_name(&self.path)?;
        // format is of the form "{ts}_{command}_{uuid}_events{ext}"
        match file_name.split('_').nth(1) {
            Some(command) => Ok(command),
            None => Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Input,
                "No command in filename"
            )),
        }
    }

    pub(crate) fn infer_opt(path: &AbsPathBuf) -> buck2_error::Result<Option<Self>> {
        let name = Self::file_name(path)?;

        for encoding in KNOWN_ENCODINGS {
            for extension in encoding.extensions {
                if name.ends_with(extension) {
                    return Ok(Some(Self {
                        path: path.clone(),
                        encoding: *encoding,
                    }));
                }
            }
        }

        Ok(None)
    }

    async fn unpack_stream_json<'a>(
        &self,
        stats: Option<&'a ReaderStats>,
    ) -> buck2_error::Result<(Invocation, BoxStream<'a, buck2_error::Result<StreamValue>>)> {
        assert_eq!(self.encoding.mode, LogMode::Json);

        let log_file = self.open(stats).await?;
        let log_file = BufReader::new(log_file);
        let mut log_lines = log_file.lines();

        // This one is not an event.
        let header = log_lines
            .next_line()
            .await
            .buck_error_context("Error reading header line")?
            .ok_or_else(|| internal_error!("No header line"))?;
        let invocation = Invocation::parse_json_line(&header)?;

        let events = LinesStream::new(log_lines).map(|line| {
            let line = line.buck_error_context("Error reading next line")?;
            serde_json::from_str::<StreamValue>(&line)
                .with_buck_error_context(|| format!("Invalid line: {}", line.trim_end()))
        });

        // Wrap in tolerant_of_truncation to handle in-progress logs
        let events = tolerant_of_truncation(events);

        Ok((invocation, events.boxed()))
    }

    async fn unpack_stream_protobuf<'a>(
        &self,
        stats: Option<&'a ReaderStats>,
    ) -> buck2_error::Result<(Invocation, BoxStream<'a, buck2_error::Result<StreamValue>>)> {
        assert_eq!(self.encoding.mode, LogMode::Protobuf);

        let log_file = self.open(stats).await?;
        let mut stream = FramedRead::new(log_file, ProtobufSplitter);

        let invocation = stream
            .try_next()
            .await?
            .ok_or_else(|| internal_error!("No invocation found"))?;
        let invocation = buck2_data::Invocation::decode_length_delimited(invocation)
            .buck_error_context("Invalid Invocation")?;
        let invocation = Invocation::from_proto(invocation);

        let events = stream.and_then(|data| async move {
            let val = buck2_cli_proto::CommandProgress::decode_length_delimited(data)
                .buck_error_context("Invalid CommandProgress")?;
            match val.progress {
                Some(command_progress::Progress::Event(event)) => Ok(StreamValue::Event(event)),
                Some(command_progress::Progress::Result(result)) => Ok(StreamValue::Result(result)),
                Some(command_progress::Progress::PartialResult(result)) => {
                    Ok(StreamValue::PartialResult(result))
                }
                None => Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::InvalidEvent,
                    "Event type not recognized"
                )),
            }
        });

        // Wrap in tolerant_of_truncation to handle in-progress logs
        let events = tolerant_of_truncation(events);

        Ok((invocation, events.boxed()))
    }

    async fn unpack_stream_inner<'a>(
        &self,
        stats: Option<&'a ReaderStats>,
    ) -> buck2_error::Result<(
        Invocation,
        impl Stream<Item = buck2_error::Result<StreamValue>> + use<'a>,
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
    ) -> buck2_error::Result<(
        Invocation,
        impl Stream<Item = buck2_error::Result<StreamValue>> + use<'a>,
    )> {
        self.unpack_stream_inner(Some(stats)).await
    }

    pub async fn unpack_stream(
        &self,
    ) -> buck2_error::Result<(
        Invocation,
        impl Stream<Item = buck2_error::Result<StreamValue>> + 'static + use<>,
    )> {
        self.unpack_stream_inner(None).await
    }

    async fn open<'a>(
        &self,
        stats: Option<&'a ReaderStats>,
    ) -> buck2_error::Result<EventLogReader<'a>> {
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

        let file = async_fs_util::open(&self.path)
            .await
            .categorize_internal()?;
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

    pub async fn get_summary(&self) -> buck2_error::Result<EventLogSummary> {
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
            .ok_or_else(|| EventLogErrors::EndOfFile(self.path.to_str().unwrap().to_owned()))?
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

#[cfg(test)]
mod tests {
    use buck2_data::CommandStart;
    use buck2_data::SpanStartEvent;
    use buck2_events::span::SpanId;
    use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;

    use super::*;
    use crate::file_names::get_logfile_name;

    #[test]
    fn test_get_uuid_from_logfile_name() -> buck2_error::Result<()> {
        // Create a test log path.
        let event = buck_event()?;
        let file_name = &get_logfile_name(&event, Encoding::PROTO_ZSTD, "bzl")?;
        let path = EventLogPathBuf {
            path: logdir().as_abs_path().join(file_name),
            encoding: Encoding::PROTO_ZSTD,
        };

        // Check we can extract UUID from log path.
        let uuid = path.uuid_from_filename().unwrap();
        assert_eq!(&uuid.to_string(), "7b797fa8-62f1-4123-85f9-875cd74b0a63");

        Ok(())
    }

    fn buck_event() -> Result<BuckEvent, buck2_error::Error> {
        let event = BuckEvent::new(
            SystemTime::now(),
            TraceId::from_str("7b797fa8-62f1-4123-85f9-875cd74b0a63")?,
            Some(SpanId::next()),
            Some(SpanId::next()),
            SpanStartEvent {
                data: Some(
                    CommandStart {
                        ..Default::default()
                    }
                    .into(),
                ),
            }
            .into(),
        );
        Ok(event)
    }

    fn logdir() -> AbsNormPathBuf {
        if cfg!(windows) {
            AbsNormPathBuf::new("C:\\foo".into()).unwrap()
        } else {
            AbsNormPathBuf::new("/foo".into()).unwrap()
        }
    }
}
