/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;
use std::time::SystemTime;

use anyhow::Context as _;
use async_compression::tokio::bufread::GzipDecoder;
use async_compression::tokio::write::GzipEncoder;
use async_trait::async_trait;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use buck2_data::buck_event;
use buck2_data::instant_event;
use chrono::offset::Utc;
use chrono::DateTime;
use cli_proto::*;
use events::subscriber::EventSubscriber;
use events::subscriber::Tick;
use events::BuckEvent;
use events::TraceId;
use futures::future::Future;
use futures::future::FutureExt;
use futures::stream::Stream;
use futures::StreamExt;
use futures::TryStreamExt;
use gazebo::dupe::Dupe;
use gazebo::prelude::*;
use prost::Message;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;
use tokio::fs::File;
use tokio::fs::OpenOptions;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio::io::AsyncWriteExt;
use tokio::io::BufReader;
use tokio_stream::wrappers::LinesStream;

use crate::daemon::client::StreamValue;
use crate::AsyncCleanupContext;

#[derive(Error, Debug)]
pub(crate) enum EventLogErrors {
    #[error(
        "Trying to write to logfile that hasn't been opened yet - this is an internal error, please report. Unwritten event: {serialized_event}"
    )]
    LogNotOpen { serialized_event: String },

    #[error("Event log does not have an implementation for reading for .proto files")]
    ReadingProtobufNotImplemented,

    #[error("Reached End of File before reading BuckEvent in log `{0}`")]
    EndOfFile(String),
}

const JSON_GZIP_EXTENSION: &str = ".json-lines.gz";
const JSON_EXTENSION: &str = ".json-lines";
const PROTO_EXTENSION: &str = ".proto";

const KNOWN_EXTENSIONS: &[(&str, Encoding)] = &[
    (JSON_GZIP_EXTENSION, Encoding::JsonGzip),
    (JSON_EXTENSION, Encoding::Json),
    (PROTO_EXTENSION, Encoding::Proto),
];

#[derive(Copy, Clone, Dupe, Debug)]
enum Encoding {
    Json,
    JsonGzip,
    Proto,
}

type EventLogWriter = Box<dyn AsyncWrite + Send + Sync + Unpin + 'static>;
type EventLogReader = Box<dyn AsyncRead + Send + Sync + Unpin + 'static>;

#[derive(Error, Debug)]
pub(crate) enum EventLogInferenceError {
    #[error("Event log at path {} has no filename", .0.display())]
    NoFilename(PathBuf),

    #[error("Event log at path {} has a non-utf-8 filename", .0.display())]
    InvalidFilename(PathBuf),

    #[error(
        "Event log at path {} has an extension that was not recognized. Valid extensions are: {}.",
        .0.display(), display_valid_extensions()
    )]
    InvalidExtension(PathBuf),
}

fn display_valid_extensions() -> String {
    let exts = KNOWN_EXTENSIONS.map(|(ext, _)| *ext);
    exts.join(", ")
}

pub(crate) struct EventLogPathBuf {
    path: PathBuf,
    encoding: Encoding,
}

pub(crate) struct EventLogSummary {
    pub(crate) trace_id: TraceId,
    pub(crate) timestamp: SystemTime,
    pub(crate) invocation: Invocation,
}

impl EventLogPathBuf {
    pub(crate) fn infer(path: PathBuf) -> anyhow::Result<Self> {
        let name = path
            .file_name()
            .with_context(|| EventLogInferenceError::NoFilename(path.clone()))?
            .to_str()
            .with_context(|| EventLogInferenceError::InvalidFilename(path.clone()))?;

        for (ext, encoding) in KNOWN_EXTENSIONS {
            if name.ends_with(ext) {
                return Ok(Self {
                    path,
                    encoding: *encoding,
                });
            }
        }

        Err(EventLogInferenceError::InvalidExtension(path.clone()).into())
    }

    /// Read the invocation line then the event stream.
    pub(crate) async fn unpack_stream(
        &self,
    ) -> anyhow::Result<(Invocation, impl Stream<Item = anyhow::Result<StreamValue>>)> {
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

        Ok((invocation, events))
    }

    async fn open(&self) -> anyhow::Result<EventLogReader> {
        tracing::info!(
            "Open {} using encoding {:?}",
            self.path.display(),
            self.encoding
        );

        let file = File::open(&self.path)
            .await
            .with_context(|| format!("Failed to open: {}", self.path.display()))?;

        let file = match self.encoding {
            Encoding::Json => box file as EventLogReader,
            Encoding::JsonGzip => box GzipDecoder::new(BufReader::new(file)) as EventLogReader,
            Encoding::Proto => {
                return Err(EventLogErrors::ReadingProtobufNotImplemented.into());
            }
        };

        Ok(file)
    }

    pub(crate) async fn get_summary(&self) -> anyhow::Result<EventLogSummary> {
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
            trace_id: buck_event.trace_id,
            timestamp: buck_event.timestamp,
            invocation,
        })
    }
}

struct NamedEventLogWriter {
    path: EventLogPathBuf,
    file: EventLogWriter,
    trace_id: TraceId,
}

enum LogFileState {
    Unopened(AbsPathBuf, Option<PathBuf>),
    Opened(Vec<NamedEventLogWriter>),
    Closed,
}

enum LogMode {
    Json,
    Protobuf,
}

/// This EventLog lets us to events emitted by Buck and log them to a file. The events are
/// serialized as JSON and logged one per line.
pub(crate) struct EventLog {
    state: LogFileState,
    async_cleanup_context: AsyncCleanupContext,
    mode: LogMode,
}

#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct Invocation {
    pub command_line_args: Vec<String>,
    pub working_dir: PathBuf,
}

impl EventLog {
    pub(crate) fn new(
        logdir: AbsPathBuf,
        extra_path: Option<PathBuf>,
        async_cleanup_context: AsyncCleanupContext,
    ) -> anyhow::Result<EventLog> {
        // The event-log is going to be written to file containing the build uuid.
        // But we don't know the build uuid until we've gotten the CommandStart event.
        // So we'll just create it when we know where to put it.
        let mut log_mode = LogMode::Json;
        static PROTOBUF_LOG: EnvHelper<bool> = EnvHelper::new("BUCK2_PROTOBUF_LOG");
        if PROTOBUF_LOG.get()?.unwrap_or(false) {
            log_mode = LogMode::Protobuf;
        }
        Ok(Self {
            state: LogFileState::Unopened(logdir, extra_path),
            async_cleanup_context,
            mode: log_mode,
        })
    }

    /// Get the command line arguments and cwd and serialize them for replaying later.
    async fn log_invocation(&mut self) -> anyhow::Result<()> {
        let working_dir = std::env::current_dir()?;
        let command_line_args = std::env::args().collect();
        let invocation = Invocation {
            command_line_args,
            working_dir,
        };
        self.write_ln(&invocation).await
    }

    async fn write_ln<'a>(&'a mut self, event: &'a impl SerializeForLog) -> anyhow::Result<()> {
        let serialized = match self.mode {
            LogMode::Json => {
                let mut serialized = event.serialize_to_json()?;
                serialized.push(b'\n');
                Ok(serialized)
            }
            LogMode::Protobuf => event.serialize_to_protobuf(),
        }?;

        match &mut self.state {
            LogFileState::Opened(files) => {
                for f in files.iter_mut() {
                    f.file
                        .write_all(&serialized)
                        .await
                        .context("Failed to write event")?;
                }
                Ok(())
            }
            LogFileState::Unopened(..) | LogFileState::Closed => Err(EventLogErrors::LogNotOpen {
                serialized_event: String::from_utf8(event.serialize_to_json()?)
                    .context("Failed to serialize event for debug")?,
            }
            .into()),
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

        // Open our log fie, gzip encoded.
        let encoding = match self.mode {
            LogMode::Json => Encoding::JsonGzip,
            LogMode::Protobuf => Encoding::Proto,
        };

        let path = EventLogPathBuf {
            path: logdir
                .join_unnormalized(get_logfile_name(event, encoding))
                .to_path_buf(),
            encoding,
        };
        let mut log_files = vec![open_event_log_for_writing(path, event.trace_id.dupe()).await?];

        // Also open the user's log file, if any as provided, with no encoding.
        if let Some(extra_path) = maybe_extra_path {
            log_files.push(
                open_event_log_for_writing(
                    EventLogPathBuf {
                        path: extra_path.clone(),
                        encoding: Encoding::Json,
                    },
                    event.trace_id.dupe(),
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
            if let Err(e) = log_upload(log_file_to_upload) {
                if e.is::<LogWasDeleted>() {
                    tracing::debug!("{}", e);
                } else {
                    tracing::warn!("Error uploading logs: {:#}", e);
                };
            }

            Ok(())
        }
    }
}

impl Drop for EventLog {
    fn drop(&mut self) {
        let exit = self.exit();
        self.async_cleanup_context.register(
            "event log upload",
            async move {
                if let Err(e) = exit.await {
                    tracing::warn!("Failed to cleanup EventLog: {:#}", e);
                }
            }
            .boxed(),
        );
    }
}

fn get_logfile_name(event: &BuckEvent, encoding: Encoding) -> ForwardRelativePathBuf {
    let time_str = {
        let datetime: DateTime<Utc> = event.timestamp.into();
        datetime.format("%Y%m%d-%H%M%S").to_string()
    };

    let extension = match encoding {
        Encoding::Json => JSON_EXTENSION,
        Encoding::JsonGzip => JSON_GZIP_EXTENSION,
        Encoding::Proto => PROTO_EXTENSION,
    };

    // Sort order matters here: earliest builds are lexicographically first and deleted first.
    ForwardRelativePathBuf::unchecked_new(format!(
        "{}_{}_events{}",
        time_str, event.trace_id, extension
    ))
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

    let file = match path.encoding {
        Encoding::Json | Encoding::Proto => box file as EventLogWriter,
        Encoding::JsonGzip => box GzipEncoder::new(file) as EventLogWriter,
    };

    Ok(NamedEventLogWriter {
        path,
        file,
        trace_id,
    })
}

async fn remove_old_logs(logdir: &Path) {
    const N_LOGS_RETAINED: usize = 10;

    if let Ok(logfiles) = get_local_logs(logdir) {
        futures::stream::iter(logfiles.into_iter().rev().skip(N_LOGS_RETAINED - 1))
            .then(async move |file| {
                // The oldest logs might be open from another concurrent build, so suppress error.
                tokio::fs::remove_file(logdir.join(file.path())).await.ok()
            })
            .collect::<Vec<_>>()
            .await;
    }
}

/// List logs in logdir, ordered from oldest to newest.
pub(crate) fn get_local_logs(logdir: &Path) -> anyhow::Result<Vec<std::fs::DirEntry>> {
    let dir = std::fs::read_dir(logdir)?;
    let mut logfiles = dir.filter_map(Result::ok).collect::<Vec<_>>();
    logfiles.sort_by_cached_key(|file| {
        // Return Unix epoch if unable to get creation time.
        if let Ok(metadata) = file.metadata() {
            if let Ok(created) = metadata.created() {
                return created;
            }
        }
        std::time::UNIX_EPOCH
    });
    Ok(logfiles)
}

#[async_trait]
impl EventSubscriber for EventLog {
    async fn handle_event(&mut self, event: &BuckEvent) -> anyhow::Result<()> {
        if let buck_event::Data::Instant(_instant) = &event.data {
            if let Some(instant_event::Data::RawOutput(_)) = _instant.data.as_ref() {
                return Ok(());
            }
        }

        self.ensure_log_files_opened(event).await?;
        let event = StreamValue::Event(buck2_data::BuckEvent::from(event.clone()));

        self.write_ln(&event).await
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

        let event = StreamValue::Result(result.clone());

        self.write_ln(&event).await?;

        self.exit().await
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
}

/// Return the place to upload logs, or None to not upload logs at all
pub(crate) fn log_upload_url() -> Option<&'static str> {
    #[cfg(feature = "extra_logging")]
    if hostcaps::is_prod() {
        Some("https://manifold.facebook.net")
    } else {
        Some("https://manifold.c2p.facebook.net")
    }
    #[cfg(not(feature = "extra_logging"))]
    None
}

#[derive(Error, Debug)]
#[error("Log file deleted before upload")]
struct LogWasDeleted;

#[cfg(unix)]
fn log_upload(log_file: &NamedEventLogWriter) -> anyhow::Result<()> {
    use std::ffi::OsString;
    use std::io::ErrorKind;
    use std::process::Stdio;

    buck2_core::facebook_only();
    let manifold_url = match log_upload_url() {
        None => return Ok(()),
        Some(x) => x,
    };

    let gzipped_log_file: Stdio = match log_file.path.encoding {
        Encoding::Json | Encoding::Proto => {
            let gzip = buck2_core::process::background_command("gzip")
                .args([
                    "--to-stdout",
                    "--keep",
                    "--no-name",
                    &format!("{}", log_file.path.path.display()),
                ])
                .stdin(Stdio::null())
                .stdout(Stdio::piped())
                .stderr(Stdio::null())
                .spawn()?;
            gzip.stdout.unwrap().into()
        }
        Encoding::JsonGzip => {
            // Note: we don't use tokio files here since we're just handing a fd to a
            // spawned process.
            match std::fs::File::open(&log_file.path.path) {
                Ok(f) => f.into(),
                Err(e) => {
                    return Err(if e.kind() == ErrorKind::NotFound {
                        LogWasDeleted.into()
                    } else {
                        e.into()
                    });
                }
            }
        }
    };

    let cert: anyhow::Result<OsString>;
    #[cfg(fbcode_build)]
    {
        cert = find_certs::find_tls_cert();
    }

    #[cfg(not(fbcode_build))]
    {
        cert = Err(anyhow::anyhow!("Disabled in Cargo builds"));
    }

    let cert = cert.context("Error finding a cert")?;

    let url = format!(
        "{}/v0/write/flat/{}.gz?bucketName=buck2_logs&apiKey=buck2_logs-key&timeoutMsec=20000",
        manifold_url, log_file.trace_id
    );

    tracing::debug!(
        "Uploading event log to `{}` using certificate `{}`",
        url,
        cert.to_string_lossy(),
    );

    // On Sandcastle we'd like to block for the sake of higher reliability uploads at the expense
    // of a bit of delay.
    let block_on_upload = std::env::var_os("SANDCASTLE").is_some();

    let mut upload = buck2_core::process::background_command("curl");
    upload.args([
        "--silent",
        "--show-error",
        "--fail",
        "-X",
        "PUT",
        "--data-binary",
        "@-", // stdin
        &url,
        "-E",
    ]);
    upload.arg(cert);
    upload.stdin(gzipped_log_file);

    if block_on_upload {
        let res = upload
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()?
            .wait_with_output()
            .context("Failed to wait for log upload")?;

        if !res.status.success() {
            let stderr = String::from_utf8_lossy(&res.stderr);
            return Err(anyhow::anyhow!(
                "Log upload exited with {}. Stderr: `{}`",
                res.status,
                stderr.trim(),
            ));
        }
    } else {
        upload.stdout(Stdio::null()).stderr(Stdio::null()).spawn()?;
    }

    Ok(())
}

#[cfg(not(unix))]
fn log_upload(log_file: &NamedEventLogWriter) -> anyhow::Result<()> {
    let _ignored = &log_file.path;
    let _ignored = &log_file.trace_id;
    Ok(())
}

pub(crate) trait SerializeForLog {
    fn serialize_to_json(&self) -> anyhow::Result<Vec<u8>>;
    fn serialize_to_protobuf(&self) -> anyhow::Result<Vec<u8>>;
}

impl SerializeForLog for Invocation {
    fn serialize_to_json(&self) -> anyhow::Result<Vec<u8>> {
        serde_json::to_vec(&self).context("Failed to serialize event")
    }

    fn serialize_to_protobuf(&self) -> anyhow::Result<Vec<u8>> {
        let invocation = buck2_data::Invocation {
            command_line_args: self.command_line_args.clone(),
            working_dir: self.working_dir.display().to_string(),
        };
        let mut res = Vec::new();
        invocation
            .encode_length_delimited(&mut res)
            .context("Failed to serialize event")?;
        Ok(res)
    }
}

impl SerializeForLog for StreamValue {
    fn serialize_to_json(&self) -> anyhow::Result<Vec<u8>> {
        serde_json::to_vec(&self).context("Failed to serialize event")
    }

    fn serialize_to_protobuf(&self) -> anyhow::Result<Vec<u8>> {
        let progress = match self {
            Self::Event(e) => command_progress::Progress::Event(e.clone()),
            Self::Result(res) => command_progress::Progress::Result(res.clone()),
        };
        let stream_val = cli_proto::CommandProgress {
            progress: Some(progress),
        };
        let mut res = Vec::new();
        stream_val
            .encode_length_delimited(&mut res)
            .context("Failed to serialize event")?;
        Ok(res)
    }
}
