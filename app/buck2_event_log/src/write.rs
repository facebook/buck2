/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;
use std::process::Stdio;
use std::sync::atomic::AtomicU64;
use std::sync::Arc;

use buck2_cli_proto::*;
use buck2_common::argv::SanitizedArgv;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::working_dir::AbsWorkingDir;
use buck2_error::BuckErrorContext;
use buck2_events::BuckEvent;
use buck2_wrapper_common::invocation_id::TraceId;
use futures::future::Future;
use prost::Message;
use serde::Serialize;
use tokio::fs::OpenOptions;

use crate::file_names::get_logfile_name;
use crate::file_names::remove_old_logs;
use crate::read::EventLogPathBuf;
use crate::should_block_on_log_upload;
use crate::should_upload_log;
use crate::user_event_types::try_get_user_event;
use crate::utils::Encoding;
use crate::utils::EventLogErrors;
use crate::utils::Invocation;
use crate::wait_for_child_and_log;
use crate::writer::EventLogType;
use crate::writer::NamedEventLogWriter;
use crate::writer::SerializeForLog;
use crate::FutureChildOutput;

enum LogWriterState {
    Unopened {
        logdir: AbsNormPathBuf,
        extra_path: Option<AbsPathBuf>,
        extra_user_event_log_path: Option<AbsPathBuf>,
    },
    Opened {
        writers: Vec<NamedEventLogWriter>,
    },
    Closed,
}

pub struct WriteEventLog {
    state: LogWriterState,
    sanitized_argv: SanitizedArgv,
    command_name: String,
    working_dir: AbsWorkingDir,
    /// Allocation cache. Must be cleaned before use.
    buf: Vec<u8>,
    log_size_counter_bytes: Option<Arc<AtomicU64>>,
}

impl WriteEventLog {
    pub fn new(
        logdir: AbsNormPathBuf,
        working_dir: AbsWorkingDir,
        extra_path: Option<AbsPathBuf>,
        extra_user_event_log_path: Option<AbsPathBuf>,
        sanitized_argv: SanitizedArgv,
        command_name: String,
        log_size_counter_bytes: Option<Arc<AtomicU64>>,
    ) -> buck2_error::Result<Self> {
        Ok(Self {
            state: LogWriterState::Unopened {
                logdir,
                extra_path,
                extra_user_event_log_path,
            },
            sanitized_argv,
            command_name,
            working_dir,
            buf: Vec::new(),
            log_size_counter_bytes,
        })
    }

    /// Get the command line arguments and cwd and serialize them for replaying later.
    async fn log_invocation(&mut self, trace_id: TraceId) -> buck2_error::Result<()> {
        let command_line_args = self.sanitized_argv.argv.clone();
        let expanded_command_line_args = self
            .sanitized_argv
            .expanded_argv
            .args()
            .map(|v| v.to_owned())
            .collect();
        let invocation = Invocation {
            command_line_args,
            expanded_command_line_args,
            working_dir: self.working_dir.to_string(),
            trace_id,
        };
        self.write_ln(&[invocation]).await
    }

    async fn write_ln<'b, T, I>(&'b mut self, events: I) -> buck2_error::Result<()>
    where
        T: SerializeForLog + 'b,
        I: IntoIterator<Item = &'b T> + Clone + 'b,
    {
        match &mut self.state {
            LogWriterState::Opened { writers, .. } => {
                for writer in writers {
                    self.buf.clear();

                    writer.write_events(&mut self.buf, &events).await?;

                    if self.buf.len() > 1_000_000 {
                        // Make sure we don't keep too much memory if encountered one large event.
                        self.buf = Vec::new();
                    }
                }
                Ok(())
            }
            LogWriterState::Unopened { .. } | LogWriterState::Closed => {
                self.buf.clear();
                if let Some(event) = events.into_iter().next() {
                    event.serialize_to_json(&mut self.buf)?;
                } else {
                    // Unreachable.
                }
                Err(EventLogErrors::LogNotOpen {
                    serialized_event: String::from_utf8(mem::take(&mut self.buf))
                        .buck_error_context("Failed to serialize event for debug")?,
                }
                .into())
            }
        }
    }

    async fn ensure_log_writers_opened(&mut self, event: &BuckEvent) -> buck2_error::Result<()> {
        let (logdir, maybe_extra_path, maybe_extra_user_event_log_path) = match &self.state {
            LogWriterState::Unopened {
                logdir,
                extra_path,
                extra_user_event_log_path,
            } => (logdir, extra_path, extra_user_event_log_path),
            LogWriterState::Opened { .. } => return Ok(()),
            LogWriterState::Closed => {
                return Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Received events after logs were closed"
                ));
            }
        };
        tokio::fs::create_dir_all(logdir)
            .await
            .with_buck_error_context(|| {
                format!("Error creating event log directory: `{}`", logdir)
            })?;
        remove_old_logs(logdir).await;

        let encoding = Encoding::PROTO_ZSTD;
        let file_name = &get_logfile_name(event, encoding, &self.command_name)?;
        let path = EventLogPathBuf {
            path: logdir.as_abs_path().join(file_name),
            encoding,
        };
        let writer = start_persist_event_log_subprocess(
            path,
            event.trace_id()?.clone(),
            self.log_size_counter_bytes.clone(),
        )
        .await?;
        let mut writers = vec![writer];

        // Also open the user's log file, if any as provided, with no encoding.
        if let Some(extra_path) = maybe_extra_path {
            writers.push(
                open_event_log_for_writing(
                    EventLogPathBuf::infer_opt(&extra_path)?.unwrap_or_else(|| EventLogPathBuf {
                        path: extra_path.clone(),
                        encoding: Encoding::JSON_GZIP,
                    }),
                    self.log_size_counter_bytes.clone(),
                    EventLogType::System,
                )
                .await?,
            );
        }

        // Also open the user's simple log file, if any as provided, json-line formatted with no compression if no extensions are detected.
        if let Some(extra_user_event_log_path) = maybe_extra_user_event_log_path {
            writers.push(
                open_event_log_for_writing(
                    EventLogPathBuf::infer_opt(&extra_user_event_log_path)?.unwrap_or_else(|| {
                        EventLogPathBuf {
                            path: extra_user_event_log_path.clone(),
                            encoding: Encoding::JSON,
                        }
                    }),
                    self.log_size_counter_bytes.clone(),
                    EventLogType::User,
                )
                .await?,
            );
        }

        self.state = LogWriterState::Opened { writers };
        self.log_invocation(event.trace_id()?).await
    }

    pub fn exit(&mut self) -> impl Future<Output = ()> + 'static + Send + Sync {
        // Shut down writers, flush all our files before exiting.
        let state = std::mem::replace(&mut self.state, LogWriterState::Closed);

        async move {
            let mut writers = match state {
                LogWriterState::Opened { writers } => writers,
                LogWriterState::Unopened { .. } | LogWriterState::Closed => {
                    // Nothing to do in this case, though this should be unreachable
                    // since we just did a write_ln.
                    return;
                }
            };

            for writer in writers.iter_mut() {
                writer.shutdown().await
            }

            // NOTE: We call `into_iter()` here and that implicitly drops the `writer.file`, which
            // is necessary for an actual `close` call to be send to the child FD (it is a bit of
            // an odd behavior in Tokio that `shutdown` doesn't do that).
            let futs = writers
                .into_iter()
                .filter_map(|w| w.child())
                .map(|proc| wait_for_child_and_log(proc, "Event Log"));

            futures::future::join_all(futs).await;
        }
    }
}

async fn start_persist_event_log_subprocess(
    path: EventLogPathBuf,
    trace_id: TraceId,
    bytes_written: Option<Arc<AtomicU64>>,
) -> buck2_error::Result<NamedEventLogWriter> {
    let current_exe = std::env::current_exe().buck_error_context("No current_exe")?;
    let mut command = buck2_util::process::async_background_command(current_exe);
    // @oss-disable: #[cfg(unix)]
    #[cfg(all(tokio_unstable, unix))] // @oss-enable
    {
        // Ensure that if we get CTRL-C, the persist-event-logs process does not get it.
        command.process_group(0);
    }
    let manifold_name = &format!("{}{}", trace_id, path.extension());
    // TODO T184566736: detach subprocess
    command
        .args(["debug", "persist-event-logs"])
        .args(["--manifold-name", manifold_name])
        .args(["--local-path".as_ref(), path.path.as_os_str()])
        .args(["--trace-id", &trace_id.to_string()]);
    if !should_upload_log()? {
        command.arg("--no-upload");
    };
    command.stdout(Stdio::null()).stdin(Stdio::piped());

    let block = should_block_on_log_upload()?;
    if block {
        command.stderr(Stdio::piped());
    } else {
        command.stderr(Stdio::null());
    }

    let mut child = command.spawn().with_buck_error_context(|| {
        format!(
            "Failed to open event log subprocess for writing at `{}`",
            path.path.display()
        )
    })?;
    let pipe = child.stdin.take().expect("stdin was piped");

    // Only spawn this if we are going to wait.
    let process_to_wait_for = if block {
        Some(FutureChildOutput::new(child))
    } else {
        None
    };

    Ok(NamedEventLogWriter::new(
        path,
        pipe,
        bytes_written,
        EventLogType::System,
        process_to_wait_for,
    ))
}

async fn open_event_log_for_writing(
    path: EventLogPathBuf,
    bytes_written: Option<Arc<AtomicU64>>,
    event_log_type: EventLogType,
) -> buck2_error::Result<NamedEventLogWriter> {
    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path.path)
        .await
        .with_buck_error_context(|| {
            format!(
                "Failed to open event log for writing at `{}`",
                path.path.display()
            )
        })?;

    Ok(NamedEventLogWriter::new(
        path,
        file,
        bytes_written,
        event_log_type,
        None,
    ))
}

impl WriteEventLog {
    pub async fn write_events(&mut self, events: &[Arc<BuckEvent>]) -> buck2_error::Result<()> {
        let mut event_refs = Vec::new();
        let mut first = true;
        for event in events {
            if first {
                self.ensure_log_writers_opened(event).await?;
                first = false;
            }

            event_refs.push(StreamValueForWrite::Event(event.event()));
        }

        if event_refs.is_empty() {
            return Ok(());
        }

        self.write_ln(&event_refs).await
    }

    pub async fn write_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> buck2_error::Result<()> {
        match &self.state {
            LogWriterState::Opened { .. } | LogWriterState::Closed => {}
            LogWriterState::Unopened { .. } => {
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

    pub async fn flush_files(&mut self) -> buck2_error::Result<()> {
        let writers = match &mut self.state {
            LogWriterState::Opened { writers } => writers,
            LogWriterState::Unopened { .. } | LogWriterState::Closed => return Ok(()),
        };

        for writer in writers {
            writer.flush().await?;
        }

        Ok(())
    }
}

impl SerializeForLog for Invocation {
    fn serialize_to_json(&self, buf: &mut Vec<u8>) -> buck2_error::Result<()> {
        serde_json::to_writer(buf, &self).buck_error_context("Failed to serialize event")
    }

    fn serialize_to_protobuf_length_delimited(&self, buf: &mut Vec<u8>) -> buck2_error::Result<()> {
        let invocation = buck2_data::Invocation {
            command_line_args: self.command_line_args.clone(),
            expanded_command_line_args: self.expanded_command_line_args.clone(),
            working_dir: self.working_dir.clone(),
            trace_id: Some(self.trace_id.to_string()),
        };
        invocation.encode_length_delimited(buf)?;
        Ok(())
    }

    // Always log invocation record to user event log for `buck2 log show` compatibility
    fn maybe_serialize_user_event(&self, buf: &mut Vec<u8>) -> buck2_error::Result<bool> {
        serde_json::to_writer(buf, &self).buck_error_context("Failed to serialize event")?;
        Ok(true)
    }
}

#[derive(Serialize)]
pub enum StreamValueForWrite<'a> {
    Result(&'a CommandResult),
    Event(&'a buck2_data::BuckEvent),
}

impl<'a> SerializeForLog for StreamValueForWrite<'a> {
    fn serialize_to_json(&self, buf: &mut Vec<u8>) -> buck2_error::Result<()> {
        serde_json::to_writer(buf, &self).buck_error_context("Failed to serialize event")
    }

    fn serialize_to_protobuf_length_delimited(&self, buf: &mut Vec<u8>) -> buck2_error::Result<()> {
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

    fn maybe_serialize_user_event(&self, buf: &mut Vec<u8>) -> buck2_error::Result<bool> {
        if let StreamValueForWrite::Event(event) = self {
            if let Some(user_event) = try_get_user_event(event)? {
                serde_json::to_writer(buf, &user_event)
                    .buck_error_context("Failed to serialize event")?;
                return Ok(true);
            }
        }

        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use std::time::SystemTime;

    use buck2_common::argv::Argv;
    use buck2_common::argv::ExpandedArgv;
    use buck2_data::LoadBuildFileStart;
    use buck2_data::SpanStartEvent;
    use buck2_events::span::SpanId;
    use futures::TryStreamExt;
    use tempfile::TempDir;

    use super::*;
    use crate::stream_value::StreamValue;
    use crate::utils::Compression;

    impl WriteEventLog {
        async fn new_test(log: EventLogPathBuf) -> buck2_error::Result<Self> {
            Ok(Self {
                state: LogWriterState::Opened {
                    writers: vec![
                        open_event_log_for_writing(log, None, EventLogType::System).await?,
                    ],
                },
                sanitized_argv: Argv {
                    argv: vec!["buck2".to_owned()],
                    expanded_argv: ExpandedArgv::from_literals(vec!["buck2".to_owned()]),
                }
                .no_need_to_sanitize(),
                command_name: "testtest".to_owned(),
                working_dir: AbsWorkingDir::current_dir()?,
                buf: Vec::new(),
                log_size_counter_bytes: None,
            })
        }
    }

    fn make_event() -> BuckEvent {
        BuckEvent::new(
            SystemTime::now(),
            TraceId::new(),
            Some(SpanId::next()),
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
    async fn test_protobuf_decoding_gzip() -> buck2_error::Result<()> {
        test_protobuf_decoding(Encoding::PROTO_GZIP).await
    }

    #[tokio::test]
    async fn test_protobuf_decoding_zstd() -> buck2_error::Result<()> {
        test_protobuf_decoding(Encoding::PROTO_ZSTD).await
    }

    async fn test_protobuf_decoding(encoding: Encoding) -> buck2_error::Result<()> {
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
        write_event_log.exit().await;

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
    async fn test_tick_makes_valid_log_zstd() -> buck2_error::Result<()> {
        test_tick_makes_valid_log(Encoding::PROTO_ZSTD).await
    }

    async fn test_tick_makes_valid_log(encoding: Encoding) -> buck2_error::Result<()> {
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
