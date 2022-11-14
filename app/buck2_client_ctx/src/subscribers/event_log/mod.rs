/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::needless_return)] // FIXME?

pub mod file_names;
pub mod upload;

use std::io::Cursor;
use std::mem;
use std::pin::Pin;
use std::sync::Arc;
use std::time::SystemTime;

use anyhow::Context as _;
use async_compression::tokio::bufread::GzipDecoder;
use async_compression::tokio::bufread::ZstdDecoder;
use async_compression::tokio::write::GzipEncoder;
use async_compression::tokio::write::ZstdEncoder;
use async_trait::async_trait;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::async_fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::working_dir::WorkingDir;
use buck2_data::buck_event;
use buck2_data::instant_event;
use buck2_events::trace::TraceId;
use buck2_events::BuckEvent;
use bytes::BytesMut;
use cli_proto::*;
use futures::future::Future;
use futures::stream::Stream;
use futures::stream::TryStreamExt;
use futures::FutureExt;
use futures::StreamExt;
use gazebo::dupe::Dupe;
use itertools::Itertools;
use prost::Message;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;
use tokio::fs::OpenOptions;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio::io::AsyncWriteExt;
use tokio::io::BufReader;
use tokio_stream::wrappers::LinesStream;
use tokio_util::codec::Decoder;
use tokio_util::codec::FramedRead;

use crate::cleanup_ctx::AsyncCleanupContext;
use crate::stream_value::StreamValue;
use crate::stream_value::StreamValueRef;
use crate::subscribers::event_log::file_names::get_logfile_name;
use crate::subscribers::event_log::file_names::remove_old_logs;
use crate::subscribers::event_log::upload::log_upload;
use crate::subscribers::event_log::upload::LogUploadError;
use crate::subscribers::subscriber::EventSubscriber;
use crate::subscribers::subscriber::Tick;

#[derive(Error, Debug)]
enum EventLogErrors {
    #[error(
        "Trying to write to logfile that hasn't been opened yet - this is an internal error, please report. Unwritten event: {serialized_event}"
    )]
    LogNotOpen { serialized_event: String },

    #[error("Reached End of File before reading BuckEvent in log `{0}`")]
    EndOfFile(String),
    #[error("No event log available for {idx}th last command (have latest {num_logfiles})")]
    RecentIndexOutOfBounds { idx: usize, num_logfiles: usize },
}

#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) struct Encoding {
    mode: LogMode,
    compression: Compression,
    /// List of extensions used to detect file type.
    ///
    /// The first extension is the default one, used when writing a file.
    extensions: &'static [&'static str],
}

impl Encoding {
    const JSON: Encoding = Encoding {
        mode: LogMode::Json,
        compression: Compression::None,
        extensions: &[".json-lines"],
    };

    const JSON_GZIP: Encoding = Encoding {
        mode: LogMode::Json,
        compression: Compression::Gzip,
        extensions: &[".json-lines.gz"],
    };

    const JSON_ZSTD: Encoding = Encoding {
        mode: LogMode::Json,
        compression: Compression::Zstd,
        extensions: &[".json-lines.zst"],
    };

    const PROTO: Encoding = Encoding {
        mode: LogMode::Protobuf,
        compression: Compression::None,
        // TODO(nga): make `.pb` default in two weeks after D40616394 lands.
        extensions: &[".proto", ".pb"],
    };

    const PROTO_GZIP: Encoding = Encoding {
        mode: LogMode::Protobuf,
        compression: Compression::Gzip,
        // TODO(nga): make `.pb.gz` default in two weeks after D40616394 lands.
        extensions: &[".proto.gz", ".pb.gz"],
    };

    const PROTO_ZSTD: Encoding = Encoding {
        mode: LogMode::Protobuf,
        compression: Compression::Zstd,
        extensions: &[".pb.zst"],
    };
}

const KNOWN_ENCODINGS: &[Encoding] = &[
    // Don't forget to update these lists when this is updated:
    // * https://fburl.com/code/zgdxtryb
    // * https://fburl.com/code/antguytj
    Encoding::JSON_GZIP,
    Encoding::JSON,
    Encoding::JSON_ZSTD,
    Encoding::PROTO,
    Encoding::PROTO_GZIP,
    Encoding::PROTO_ZSTD,
];

type EventLogWriter = Box<dyn AsyncWrite + Send + Sync + Unpin + 'static>;
type EventLogReader = Box<dyn AsyncRead + Send + Sync + Unpin + 'static>;

#[derive(Error, Debug)]
enum EventLogInferenceError {
    #[error("Event log at path {} has no filename", .0.display())]
    NoFilename(AbsPathBuf),

    #[error("Event log at path {} has a non-utf-8 filename", .0.display())]
    InvalidFilename(AbsPathBuf),

    #[error(
        "Event log at path {} has an extension that was not recognized. Valid extensions are: {}.",
        .0.display(), display_valid_extensions()
    )]
    InvalidExtension(AbsPathBuf),
}

fn display_valid_extensions() -> String {
    let mut exts = KNOWN_ENCODINGS
        .iter()
        .flat_map(|encoding| encoding.extensions);
    exts.join(", ")
}

#[derive(Clone)]
pub struct EventLogPathBuf {
    path: AbsPathBuf,
    encoding: Encoding,
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

    fn infer_opt(path: AbsPathBuf) -> anyhow::Result<Result<Self, NoInference>> {
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

    async fn unpack_stream_json(
        &self,
    ) -> anyhow::Result<(
        Invocation,
        Pin<Box<dyn Stream<Item = anyhow::Result<StreamValue>> + Send>>,
    )> {
        assert_eq!(self.encoding.mode, LogMode::Json);

        let log_file = self.open().await?;
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

    async fn unpack_stream_protobuf(
        &self,
    ) -> anyhow::Result<(
        Invocation,
        Pin<Box<dyn Stream<Item = anyhow::Result<StreamValue>> + Send>>,
    )> {
        assert_eq!(self.encoding.mode, LogMode::Protobuf);

        let log_file = self.open().await?;
        let mut stream = FramedRead::new(log_file, EventLogDecoder::new());

        let invocation = match stream.try_next().await?.context("No invocation found")? {
            Frame::Invocation(inv) => Invocation {
                command_line_args: inv.command_line_args,
                working_dir: inv.working_dir,
            },
            Frame::Value(_) => {
                return Err(anyhow::anyhow!("Expected Invocation, found StreamValue"));
            }
        };

        let events = stream.and_then(|frame| async move {
            match frame {
                Frame::Invocation(_) => {
                    Err(anyhow::anyhow!("Expected StreamValue, found Invocation"))
                }
                Frame::Value(val) => match val.progress {
                    Some(command_progress::Progress::Event(event)) => Ok(StreamValue::Event(event)),
                    Some(command_progress::Progress::Result(result)) => {
                        Ok(StreamValue::Result(result))
                    }
                    None => return Err(anyhow::anyhow!("Event type not recognized")),
                },
            }
        });

        Ok((invocation, events.boxed()))
    }

    /// Read the invocation line then the event stream.
    pub async fn unpack_stream(
        &self,
    ) -> anyhow::Result<(Invocation, impl Stream<Item = anyhow::Result<StreamValue>>)> {
        match self.encoding.mode {
            LogMode::Json => self.unpack_stream_json().await,
            LogMode::Protobuf => self.unpack_stream_protobuf().await,
        }
    }

    async fn open(&self) -> anyhow::Result<EventLogReader> {
        tracing::info!(
            "Open {} using encoding {:?}",
            self.path.display(),
            self.encoding
        );

        let file = async_fs_util::open(&self.path).await?;
        let file = match self.encoding.compression {
            Compression::None => box file as EventLogReader,
            Compression::Gzip => box GzipDecoder::new(BufReader::new(file)) as EventLogReader,
            Compression::Zstd => box ZstdDecoder::new(BufReader::new(file)) as EventLogReader,
        };

        Ok(file)
    }

    pub async fn get_summary(&self) -> anyhow::Result<EventLogSummary> {
        let (invocation, events) = self.unpack_stream().await?;
        let buck_event: BuckEvent = events
            .try_filter_map(|log| {
                let maybe_buck_event = match log {
                    StreamValue::Result(_) => None,
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
}

struct NoInference(AbsPathBuf);

struct NamedEventLogWriter {
    path: EventLogPathBuf,
    file: EventLogWriter,
    trace_id: TraceId,
}

enum LogFileState {
    Unopened(AbsNormPathBuf, Option<AbsPathBuf>),
    Opened(Vec<NamedEventLogWriter>),
    Closed,
}

#[derive(Copy, Clone, Dupe, Debug, PartialEq, Eq)]
enum LogMode {
    Json,
    Protobuf,
}

#[derive(Copy, Clone, Dupe, Debug)]
enum Compression {
    None,
    Gzip,
    Zstd,
}

/// This EventLog lets us to events emitted by Buck and log them to a file. The events are
/// serialized as JSON and logged one per line.
pub(crate) struct EventLog {
    state: LogFileState,
    async_cleanup_context: Option<AsyncCleanupContext>,
    sanitized_argv: Vec<String>,
    command_name: String,
    working_dir: WorkingDir,
    /// Allocation cache. Must be cleaned before use.
    buf: Vec<u8>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Invocation {
    pub command_line_args: Vec<String>,
    pub working_dir: String,
}

impl EventLog {
    pub(crate) fn new(
        logdir: AbsNormPathBuf,
        working_dir: WorkingDir,
        extra_path: Option<AbsPathBuf>,
        sanitized_argv: Vec<String>,
        async_cleanup_context: AsyncCleanupContext,
        command_name: String,
    ) -> anyhow::Result<EventLog> {
        Ok(Self {
            state: LogFileState::Unopened(logdir, extra_path),
            async_cleanup_context: Some(async_cleanup_context),
            sanitized_argv,
            command_name,
            working_dir,
            buf: Vec::new(),
        })
    }

    /// Get the command line arguments and cwd and serialize them for replaying later.
    async fn log_invocation(&mut self) -> anyhow::Result<()> {
        let command_line_args = self.sanitized_argv.clone();
        let invocation = Invocation {
            command_line_args,
            working_dir: self.working_dir.to_string(),
        };
        self.write_ln(&[invocation]).await
    }

    async fn write_ln<'a, T, I>(&'a mut self, events: I) -> anyhow::Result<()>
    where
        T: SerializeForLog + 'a,
        I: IntoIterator<Item = &'a T> + Clone + 'a,
    {
        match &mut self.state {
            LogFileState::Opened(files) => {
                for f in files.iter_mut() {
                    self.buf.clear();

                    for event in events.clone() {
                        match f.path.encoding.mode {
                            LogMode::Json => {
                                event.serialize_to_json(&mut self.buf)?;
                                self.buf.push(b'\n');
                            }
                            LogMode::Protobuf => {
                                event.serialize_to_protobuf_length_delimited(&mut self.buf)?
                            }
                        };
                    }

                    f.file
                        .write_all(&self.buf)
                        .await
                        .context("Failed to write event")?;

                    if self.buf.len() > 1_000_000 {
                        // Make sure we don't keep too much memory if encountered one large event.
                        self.buf = Vec::new();
                    }
                }
                Ok(())
            }
            LogFileState::Unopened(..) | LogFileState::Closed => {
                self.buf.clear();
                if let Some(event) = events.into_iter().next() {
                    event.serialize_to_json(&mut self.buf)?;
                } else {
                    // Unreachable.
                }
                Err(EventLogErrors::LogNotOpen {
                    serialized_event: String::from_utf8(mem::take(&mut self.buf))
                        .context("Failed to serialize event for debug")?,
                }
                .into())
            }
        }
    }

    async fn ensure_log_files_opened(&mut self, event: &BuckEvent) -> anyhow::Result<()> {
        let (logdir, maybe_extra_path) = match &self.state {
            LogFileState::Unopened(logdir, extra_path) => (logdir, extra_path),
            LogFileState::Opened(_) => return Ok(()),
            LogFileState::Closed => {
                return Err(anyhow::anyhow!("Received events after logs were closed"));
            }
        };
        tokio::fs::create_dir_all(logdir)
            .await
            .with_context(|| format!("Error creating event log directory: `{}`", logdir))?;
        remove_old_logs(logdir).await;

        // The event-log is going to be written to file containing the build uuid.
        // But we don't know the build uuid until we've gotten the CommandStart event.
        // So we'll just create it when we know where to put it.
        let mut log_mode = LogMode::Protobuf;
        static JSON_LOG: EnvHelper<bool> = EnvHelper::new("BUCK2_JSON_LOG");
        if JSON_LOG.get_copied()?.unwrap_or(false) {
            log_mode = LogMode::Json;
        }

        // Open our log fie, gzip encoded.
        let encoding = match log_mode {
            LogMode::Json => Encoding::JSON_GZIP,
            LogMode::Protobuf => Encoding::PROTO_ZSTD,
        };

        let path = EventLogPathBuf {
            path: logdir
                .as_abs_path()
                .join(get_logfile_name(event, encoding, &self.command_name)?),
            encoding,
        };
        let mut log_files = vec![open_event_log_for_writing(path, event.trace_id()?).await?];

        // Also open the user's log file, if any as provided, with no encoding.
        if let Some(extra_path) = maybe_extra_path {
            log_files.push(
                open_event_log_for_writing(
                    EventLogPathBuf::infer_opt(extra_path.clone())?.unwrap_or_else(
                        |NoInference(path)| EventLogPathBuf {
                            path,
                            encoding: Encoding::JSON_GZIP,
                        },
                    ),
                    event.trace_id()?,
                )
                .await?,
            );
        }

        self.state = LogFileState::Opened(log_files);
        self.log_invocation().await
    }

    fn exit(&mut self) -> impl Future<Output = anyhow::Result<()>> + 'static + Send + Sync {
        // Flush all our files before exiting.
        let mut log_files = match &mut self.state {
            LogFileState::Opened(files) => std::mem::take(files),
            LogFileState::Unopened(..) | LogFileState::Closed => {
                // Nothing to do in this case, though this should be unreachable since we just did
                // a write_ln.
                vec![]
            }
        };

        self.state = LogFileState::Closed;

        async move {
            for file in log_files.iter_mut() {
                file.file.shutdown().await?;
            }

            let log_file_to_upload = match log_files.first() {
                Some(log) => log,
                None => return Ok(()),
            };

            // NOTE: we ignore outputs here so that we don't fail if e.g. something deleted our log
            // file while we were about to upload it.
            if let Err(e) = log_upload(&log_file_to_upload.path, &log_file_to_upload.trace_id).await
            {
                match e {
                    e @ LogUploadError::LogWasDeleted => {
                        tracing::debug!("{}", e);
                    }
                    LogUploadError::Other(e) => {
                        tracing::warn!("Error uploading logs: {:#}", e);
                    }
                }
            }

            Ok(())
        }
    }
}

impl Drop for EventLog {
    fn drop(&mut self) {
        let exit = self.exit();
        match self.async_cleanup_context.as_ref() {
            Some(async_cleanup_context) => {
                async_cleanup_context.register(
                    "event log upload",
                    async move {
                        if let Err(e) = exit.await {
                            tracing::warn!("Failed to cleanup EventLog: {:#}", e);
                        }
                    }
                    .boxed(),
                );
            }
            None => (),
        }
    }
}

async fn open_event_log_for_writing(
    path: EventLogPathBuf,
    trace_id: TraceId,
) -> anyhow::Result<NamedEventLogWriter> {
    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path.path)
        .await
        .with_context(|| {
            format!(
                "Failed to open event log for writing at `{}`",
                path.path.display()
            )
        })?;

    let file = match path.encoding.compression {
        Compression::None => box file as EventLogWriter,
        Compression::Gzip => {
            box GzipEncoder::with_quality(file, async_compression::Level::Fastest) as EventLogWriter
        }
        Compression::Zstd => {
            box ZstdEncoder::with_quality(file, async_compression::Level::Default) as EventLogWriter
        }
    };

    Ok(NamedEventLogWriter {
        path,
        file,
        trace_id,
    })
}

#[async_trait]
impl EventSubscriber for EventLog {
    async fn handle_events(&mut self, events: &[Arc<BuckEvent>]) -> anyhow::Result<()> {
        let mut event_refs = Vec::new();
        let mut first = true;
        for event in events {
            if let buck_event::Data::Instant(instant) = event.data() {
                if let Some(instant_event::Data::RawOutput(_)) = instant.data.as_ref() {
                    continue;
                }
            }
            if first {
                self.ensure_log_files_opened(event).await?;
                first = false;
            }
            event_refs.push(StreamValueRef::Event(event.event()));
        }

        if event_refs.is_empty() {
            return Ok(());
        }

        self.write_ln(&event_refs).await
    }

    async fn handle_command_result(
        &mut self,
        result: &cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        match &self.state {
            LogFileState::Opened(..) | LogFileState::Closed => {}
            LogFileState::Unopened(..) => {
                // This is a bit wonky. We can receive a CommandResult before we opened log files
                // if the command crashed before it started. That can happen if the daemon
                // initialization is what fails, since we need the daemon to initialize in order to
                // access request metadata, which we need for the command start event. To keep
                // things simple, just tolerate this happening.
                return Ok(());
            }
        }

        let event = StreamValueRef::Result(result);

        self.write_ln(&[event]).await
    }

    /// Flush all log files during on tick to avoid buffering data in memory which we might lose if
    /// we hit an error.
    async fn tick(&mut self, _tick: &Tick) -> anyhow::Result<()> {
        let log_files = match &mut self.state {
            LogFileState::Opened(files) => files,
            LogFileState::Unopened(..) | LogFileState::Closed => return Ok(()),
        };

        for file in log_files {
            file.file.flush().await.with_context(|| {
                format!("Error flushing log file at {}", file.path.path.display())
            })?;
        }

        Ok(())
    }

    async fn exit(&mut self) -> anyhow::Result<()> {
        self.exit().await
    }
}

pub trait SerializeForLog {
    fn serialize_to_json(&self, buf: &mut Vec<u8>) -> anyhow::Result<()>;
    fn serialize_to_protobuf_length_delimited(&self, buf: &mut Vec<u8>) -> anyhow::Result<()>;
}

impl SerializeForLog for Invocation {
    fn serialize_to_json(&self, buf: &mut Vec<u8>) -> anyhow::Result<()> {
        serde_json::to_writer(buf, &self).context("Failed to serialize event")
    }

    fn serialize_to_protobuf_length_delimited(&self, buf: &mut Vec<u8>) -> anyhow::Result<()> {
        let invocation = buck2_data::Invocation {
            command_line_args: self.command_line_args.clone(),
            working_dir: self.working_dir.clone(),
        };
        invocation.encode_length_delimited(buf)?;
        Ok(())
    }
}

impl<'a> SerializeForLog for StreamValueRef<'a> {
    fn serialize_to_json(&self, buf: &mut Vec<u8>) -> anyhow::Result<()> {
        serde_json::to_writer(buf, &self).context("Failed to serialize event")
    }

    fn serialize_to_protobuf_length_delimited(&self, buf: &mut Vec<u8>) -> anyhow::Result<()> {
        // We use `CommandProgressForWrite` here to avoid cloning `BuckEvent`.
        // `CommandProgressForWrite` serialization is bitwise identical to `CommandProgress`.
        // See the protobuf spec
        // https://developers.google.com/protocol-buffers/docs/encoding#length-types
        // for the details about protobuf wire format.
        let progress = match self {
            Self::Event(e) => command_progress_for_write::Progress::Event(e.encode_to_vec()),
            Self::Result(res) => command_progress_for_write::Progress::Result((*res).clone()),
        };
        let stream_val = cli_proto::CommandProgressForWrite {
            progress: Some(progress),
        };
        stream_val.encode_length_delimited(buf)?;
        Ok(())
    }
}

#[allow(clippy::large_enum_variant)]
enum Frame {
    Invocation(buck2_data::Invocation),
    Value(cli_proto::CommandProgress),
}

struct EventLogDecoder {
    saw_invocation: bool,
}

impl EventLogDecoder {
    pub(crate) fn new() -> EventLogDecoder {
        Self {
            saw_invocation: false,
        }
    }
}
impl Decoder for EventLogDecoder {
    type Item = Frame;
    type Error = anyhow::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        let orig_len = src.len();

        let data_length = match prost::decode_length_delimiter(Cursor::new(src.as_mut())) {
            Ok(length) => length,
            Err(..) => {
                // 10 bytes is the largest length of an encoded size
                if orig_len > 10 {
                    return Err(anyhow::anyhow!("Corrupted stream"));
                } else {
                    return Ok(None);
                }
            }
        };

        let required_len = prost::length_delimiter_len(data_length) + data_length;
        if orig_len < required_len {
            return Ok(None);
        }
        let data = src.split_to(required_len);

        Some(
            if self.saw_invocation {
                cli_proto::CommandProgress::decode_length_delimited(data).map(Frame::Value)
            } else {
                self.saw_invocation = true;
                buck2_data::Invocation::decode_length_delimited(data).map(Frame::Invocation)
            }
            .context("Failed to decode"),
        )
        .transpose()
    }
}

#[cfg(test)]
mod tests {
    use buck2_data::LoadBuildFileStart;
    use buck2_data::SpanStartEvent;
    use buck2_events::span::SpanId;
    use futures::TryStreamExt;
    use tempfile::TempDir;

    use super::*;
    use crate::stream_value::StreamValue;

    impl EventLog {
        async fn new_test_event_log(log: EventLogPathBuf) -> anyhow::Result<Self> {
            Ok(Self {
                state: LogFileState::Opened(vec![
                    open_event_log_for_writing(log, TraceId::new()).await?,
                ]),
                sanitized_argv: vec!["buck2".to_owned()],
                async_cleanup_context: None,
                command_name: "testtest".to_owned(),
                working_dir: WorkingDir::current_dir()?,
                buf: Vec::new(),
            })
        }
    }

    fn make_event() -> BuckEvent {
        BuckEvent::new(
            SystemTime::now(),
            TraceId::new(),
            Some(SpanId::new()),
            None,
            buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Load(
                    LoadBuildFileStart {
                        module_id: "foo".to_owned(),
                        cell: "bar".to_owned(),
                    },
                )),
            }),
        )
    }

    #[tokio::test]
    async fn test_protobuf_decoding_gzip() -> anyhow::Result<()> {
        test_protobuf_decoding(Encoding::PROTO_GZIP).await
    }

    #[tokio::test]
    async fn test_protobuf_decoding_zstd() -> anyhow::Result<()> {
        test_protobuf_decoding(Encoding::PROTO_ZSTD).await
    }

    async fn test_protobuf_decoding(encoding: Encoding) -> anyhow::Result<()> {
        //Create log dir
        let tmp_dir = TempDir::new()?;

        //Create mock event
        let event = make_event();

        // Create event log
        let log = EventLogPathBuf {
            path: AbsPathBuf::try_from(tmp_dir.path().join("log")).unwrap(),
            encoding,
        };

        let mut event_log = EventLog::new_test_event_log(log.clone()).await?;

        //Log event
        let value = StreamValueRef::Event(event.event());
        event_log.log_invocation().await?;
        event_log.write_ln(&[value]).await?;
        event_log.exit().await?;

        //Get and decode log
        let (_invocation, mut events) = log.unpack_stream().await?;

        //Get event
        let retrieved_event = match events.try_next().await?.expect("Failed getting log") {
            StreamValue::Event(e) => BuckEvent::try_from(e),
            StreamValue::Result(_) => panic!("found result"),
        }?;

        //Assert it's the same event created in the beginning
        assert_eq!(retrieved_event.timestamp(), event.timestamp());
        assert_eq!(
            retrieved_event.trace_id().unwrap(),
            event.trace_id().unwrap()
        );
        assert_eq!(retrieved_event.span_id().unwrap(), event.span_id().unwrap());
        assert_eq!(retrieved_event.data(), event.data());

        assert!(
            events.try_next().await.unwrap().is_none(),
            "expecting no more events"
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_tick_makes_valid_log_gzip() -> anyhow::Result<()> {
        test_tick_makes_valid_log(Encoding::PROTO_GZIP).await
    }

    #[tokio::test]
    async fn test_tick_makes_valid_log_zstd() -> anyhow::Result<()> {
        test_tick_makes_valid_log(Encoding::PROTO_ZSTD).await
    }

    async fn test_tick_makes_valid_log(encoding: Encoding) -> anyhow::Result<()> {
        if cfg!(windows) {
            // Do not want to deal with exclusivity issues on Windows.
            return Ok(());
        }

        let tmp_dir = TempDir::new()?;

        let log = EventLogPathBuf {
            path: AbsPathBuf::try_from(tmp_dir.path().join("test_tick_makes_valid_log.pb.gz"))
                .unwrap(),
            encoding,
        };

        let mut event_log = EventLog::new_test_event_log(log.clone()).await?;

        let event = make_event();
        let value = StreamValueRef::Event(event.event());
        event_log.log_invocation().await?;
        event_log.write_ln(&[value]).await?;

        assert!(
            log.unpack_stream().await.is_err(),
            "Sanity check: gzip was not flushed, so the log is invalid"
        );

        // Now flush the gzip stream.
        event_log.tick(&Tick::now()).await?;

        // Do not close the log, and open it.
        let (_invocation, mut events) = log.unpack_stream().await?;

        let retrieved_event = match events.try_next().await?.expect("Failed getting log") {
            StreamValue::Event(e) => BuckEvent::try_from(e).unwrap(),
            StreamValue::Result(_) => panic!("expecting event"),
        };

        assert_eq!(retrieved_event.timestamp(), event.timestamp());
        assert_eq!(
            retrieved_event.trace_id().unwrap(),
            event.trace_id().unwrap()
        );
        assert_eq!(retrieved_event.span_id(), event.span_id());
        assert_eq!(retrieved_event.data(), event.data());

        match encoding.compression {
            Compression::Gzip => {
                // TODO(nga): `tick` does not write gzip footer, so even after `tick`
                //   generated file is not a valid gzip file.
                // assert!(events.try_next().await.unwrap().is_none(), "expecting no more events");
                assert!(events.try_next().await.is_err());
            }
            Compression::Zstd => {
                assert!(
                    events.try_next().await.unwrap().is_none(),
                    "expecting no more events"
                );
            }
            Compression::None => unreachable!(),
        }

        Ok(())
    }

    #[test]
    fn test_stream_value_serialize_to_protobuf_length_delimited() {
        let event = make_event();
        let mut actual = Vec::new();
        StreamValueRef::Event(event.event())
            .serialize_to_protobuf_length_delimited(&mut actual)
            .unwrap();
        let expected = cli_proto::CommandProgress {
            progress: Some(command_progress::Progress::Event(
                buck2_data::BuckEvent::from(event),
            )),
        }
        .encode_length_delimited_to_vec();
        assert_eq!(expected, actual);
    }
}
