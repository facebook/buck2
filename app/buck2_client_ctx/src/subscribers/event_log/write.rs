/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;
use std::sync::Arc;

use anyhow::Context as _;
use async_compression::tokio::write::GzipEncoder;
use async_compression::tokio::write::ZstdEncoder;
use buck2_cli_proto::*;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::working_dir::WorkingDir;
use buck2_core::soft_error;
use buck2_events::trace::TraceId;
use buck2_events::BuckEvent;
use futures::future::Future;
use futures::FutureExt;
use prost::Message;
use serde::Serialize;
use tokio::fs::OpenOptions;
use tokio::io::AsyncWrite;
use tokio::io::AsyncWriteExt;

use crate::cleanup_ctx::AsyncCleanupContext;
use crate::subscribers::event_log::file_names::get_logfile_name;
use crate::subscribers::event_log::file_names::remove_old_logs;
use crate::subscribers::event_log::read::EventLogPathBuf;
use crate::subscribers::event_log::upload::log_upload;
use crate::subscribers::event_log::upload::LogUploadError;
use crate::subscribers::event_log::utils::Compression;
use crate::subscribers::event_log::utils::Encoding;
use crate::subscribers::event_log::utils::EventLogErrors;
use crate::subscribers::event_log::utils::Invocation;
use crate::subscribers::event_log::utils::LogMode;
use crate::subscribers::event_log::utils::NoInference;

type EventLogWriter = Box<dyn AsyncWrite + Send + Sync + Unpin + 'static>;

pub(crate) struct NamedEventLogWriter {
    path: EventLogPathBuf,
    file: EventLogWriter,
    trace_id: TraceId,
}

pub(crate) enum LogFileState {
    Unopened(AbsNormPathBuf, Option<AbsPathBuf>),
    Opened(Vec<NamedEventLogWriter>),
    Closed,
}

pub(crate) struct WriteEventLog {
    state: LogFileState,
    async_cleanup_context: Option<AsyncCleanupContext>,
    sanitized_argv: Vec<String>,
    command_name: String,
    working_dir: WorkingDir,
    /// Allocation cache. Must be cleaned before use.
    buf: Vec<u8>,
}

impl WriteEventLog {
    pub(crate) fn new(
        logdir: AbsNormPathBuf,
        working_dir: WorkingDir,
        extra_path: Option<AbsPathBuf>,
        sanitized_argv: Vec<String>,
        async_cleanup_context: AsyncCleanupContext,
        command_name: String,
    ) -> anyhow::Result<Self> {
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
    async fn log_invocation(&mut self, trace_id: TraceId) -> anyhow::Result<()> {
        let command_line_args = self.sanitized_argv.clone();
        let invocation = Invocation {
            command_line_args,
            working_dir: self.working_dir.to_string(),
            trace_id,
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
        self.log_invocation(event.trace_id()?).await
    }

    pub(crate) fn exit(
        &mut self,
    ) -> impl Future<Output = anyhow::Result<()>> + 'static + Send + Sync {
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
                if matches!(e, LogUploadError::LogWasDeleted) {
                    // This is expected to happen if more than 10 commands are run in parallel,
                    // since we only keep the 10 most recent logs
                    tracing::debug!("{}", e);
                } else {
                    // Do not fail e2e tests if manifold is not available.
                    // TODO(nga): there's no ready to use API to send such warning to Scuba,
                    //   so use `soft_error!` for now.
                    let _ignore = soft_error!("event_log_upload_failed", anyhow::Error::new(e));
                }
            }

            Ok(())
        }
    }
}

impl Drop for WriteEventLog {
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
        Compression::None => Box::new(file) as EventLogWriter,
        Compression::Gzip => Box::new(GzipEncoder::with_quality(
            file,
            async_compression::Level::Fastest,
        )) as EventLogWriter,
        Compression::Zstd => Box::new(ZstdEncoder::with_quality(
            file,
            async_compression::Level::Default,
        )) as EventLogWriter,
    };

    Ok(NamedEventLogWriter {
        path,
        file,
        trace_id,
    })
}

impl WriteEventLog {
    pub(crate) async fn write_events(&mut self, events: &[Arc<BuckEvent>]) -> anyhow::Result<()> {
        let mut event_refs = Vec::new();
        let mut first = true;
        for event in events {
            if first {
                self.ensure_log_files_opened(event).await?;
                first = false;
            }

            event_refs.push(StreamValueForWrite::Event(event.event()));
        }

        if event_refs.is_empty() {
            return Ok(());
        }

        self.write_ln(&event_refs).await
    }

    pub(crate) async fn write_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
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

        let event = StreamValueForWrite::Result(result);

        self.write_ln(&[event]).await
    }

    pub(crate) async fn flush_files(&mut self) -> anyhow::Result<()> {
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
}

trait SerializeForLog {
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
            trace_id: Some(self.trace_id.to_string()),
        };
        invocation.encode_length_delimited(buf)?;
        Ok(())
    }
}

#[derive(Serialize)]
pub enum StreamValueForWrite<'a> {
    Result(&'a CommandResult),
    Event(&'a buck2_data::BuckEvent),
}

impl<'a> SerializeForLog for StreamValueForWrite<'a> {
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
        let stream_val = buck2_cli_proto::CommandProgressForWrite {
            progress: Some(progress),
        };
        stream_val.encode_length_delimited(buf)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::time::SystemTime;

    use buck2_core::fs::paths::abs_path::AbsPathBuf;
    use buck2_data::LoadBuildFileStart;
    use buck2_data::SpanStartEvent;
    use buck2_events::span::SpanId;
    use buck2_events::trace::TraceId;
    use buck2_events::BuckEvent;
    use futures::TryStreamExt;
    use tempfile::TempDir;

    use super::*;
    use crate::stream_value::StreamValue;

    impl WriteEventLog {
        async fn new_test(log: EventLogPathBuf) -> anyhow::Result<Self> {
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

        let mut write_event_log = WriteEventLog::new_test(log.clone()).await?;

        //Log event
        let value = StreamValueForWrite::Event(event.event());
        write_event_log.log_invocation(event.trace_id()?).await?;
        write_event_log.write_ln(&[value]).await?;
        write_event_log.exit().await?;

        //Get and decode log
        let (_invocation, mut events) = log.unpack_stream().await?;

        //Get event
        let retrieved_event = match events.try_next().await?.expect("Failed getting log") {
            StreamValue::Event(e) => BuckEvent::try_from(e),
            _ => panic!("expected event"),
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

        let mut write_event_log = WriteEventLog::new_test(log.clone()).await?;

        let event = make_event();
        let value = StreamValueForWrite::Event(event.event());
        write_event_log.log_invocation(event.trace_id()?).await?;
        write_event_log.write_ln(&[value]).await?;

        assert!(
            log.unpack_stream().await.is_err(),
            "Sanity check: gzip was not flushed, so the log is invalid"
        );

        // Now flush the gzip stream.
        write_event_log.flush_files().await?;

        // Do not close the log, and open it.
        let (_invocation, mut events) = log.unpack_stream().await?;

        let retrieved_event = match events.try_next().await?.expect("Failed getting log") {
            StreamValue::Event(e) => BuckEvent::try_from(e).unwrap(),
            _ => panic!("expecting event"),
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
        StreamValueForWrite::Event(event.event())
            .serialize_to_protobuf_length_delimited(&mut actual)
            .unwrap();
        let expected = buck2_cli_proto::CommandProgress {
            progress: Some(command_progress::Progress::Event(event.into())),
        }
        .encode_length_delimited_to_vec();
        assert_eq!(expected, actual);
    }
}
