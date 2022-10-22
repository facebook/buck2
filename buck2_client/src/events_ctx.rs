/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::ControlFlow;

use anyhow::Context;
use async_trait::async_trait;
use buck2_common::daemon_dir::DaemonDir;
use cli_proto::command_result;
use cli_proto::CommandResult;
use futures::Future;
use futures::Stream;
use futures::StreamExt;
use thiserror::Error;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedReceiver;

use crate::command_outcome::CommandOutcome;
use crate::console_interaction_stream::ConsoleInteraction;
use crate::console_interaction_stream::ConsoleInteractionStream;
use crate::console_interaction_stream::NoopConsoleInteraction;
use crate::file_tailer::FileTailer;
use crate::stream_value::StreamValue;
use crate::subscribers::subscriber::EventSubscriber;
use crate::subscribers::subscriber::Tick;
use crate::ticker::Ticker;

/// Target number of self.tick() calls per second. These can be used by implementations for regular updates, for example
/// superconsole uses it to re-render the frame and this is what allows it to have constantly updating timers.
/// Other than tick() calls, implementations will only be notified when new events arrive.
const TICKS_PER_SECOND: u32 = 10;

#[derive(Debug, Error)]
#[allow(clippy::large_enum_variant)]
enum BuckdCommunicationError {
    #[error("call to daemon returned an unexpected result type. got `{0:?}`")]
    UnexpectedResultType(command_result::Result),
    #[error("buck daemon returned an empty CommandResult")]
    EmptyCommandResult,
    #[error("buck daemon request finished without returning a CommandResult")]
    MissingCommandResult,
    #[error("buckd communication encountered an unexpected error `{0:?}`")]
    TonicError(tonic::Status),
}

impl From<tonic::Status> for BuckdCommunicationError {
    fn from(status: tonic::Status) -> Self {
        match status.code() {
            tonic::Code::Ok => {
                unreachable!("::Ok should be unreachable as it should produce an Ok result")
            }
            // all errors should be encoded into the CommandResult, we must've hit something strange to be here.
            _ => BuckdCommunicationError::TonicError(status),
        }
    }
}

/// Manages incoming event streams from the daemon for the buck2 client and
/// fowards them to the appropriate subscribers registered on this struct
pub(crate) struct EventsCtx {
    pub(crate) daemon_dir: DaemonDir,
    pub(crate) subscribers: Vec<Box<dyn EventSubscriber>>,
    ticker: Ticker,
}

pub struct FileStreams {
    stdout: UnboundedReceiver<String>,
    stderr: UnboundedReceiver<String>,
}

pub(crate) enum FileTailerEvent {
    Stdout(String),
    Stderr(String),
}

impl FileStreams {
    pub(crate) async fn next(&mut self) -> Option<FileTailerEvent> {
        tokio::select! {
            Some(stdout) = self.stdout.recv() => {
                Some(FileTailerEvent::Stdout(stdout))
            }
            Some(stderr) = self.stderr.recv() => {
                Some(FileTailerEvent::Stderr(stderr))
            }
            else => None,
        }
    }
}

pub struct FileTailers {
    _stdout_tailer: Option<FileTailer>,
    _stderr_tailer: Option<FileTailer>,
    streams: FileStreams,
}

impl EventsCtx {
    pub(crate) fn new(daemon_dir: DaemonDir, subscribers: Vec<Box<dyn EventSubscriber>>) -> Self {
        Self {
            daemon_dir,
            subscribers,
            ticker: Ticker::new(TICKS_PER_SECOND),
        }
    }

    async fn handle_stream_next(
        &mut self,
        next: Option<anyhow::Result<StreamValue>>,
    ) -> anyhow::Result<ControlFlow<anyhow::Result<CommandResult>, ()>> {
        let next: anyhow::Result<_> = try {
            next.context(BuckdCommunicationError::MissingCommandResult)?
                .context("Buck daemon event bus encountered an error")?
        };
        match next {
            Ok(StreamValue::Event(event)) => {
                let event = event.try_into()?;
                self.handle_event(&event).await?;
                Ok(ControlFlow::Continue(()))
            }
            Ok(StreamValue::Result(res)) => {
                self.handle_command_result(&res).await?;
                Ok(ControlFlow::Break(Ok(res)))
            }
            Err(e) => Ok(ControlFlow::Break(Err(e))),
        }
    }

    async fn dispatch_tailer_event(&mut self, event: FileTailerEvent) -> anyhow::Result<()> {
        match event {
            FileTailerEvent::Stdout(stdout) => self.handle_output(&stdout).await,
            FileTailerEvent::Stderr(line) => self.handle_stderr(line.trim_end()).await,
        }
    }

    async fn unpack_stream_inner<S: Stream<Item = anyhow::Result<StreamValue>> + Unpin>(
        &mut self,
        stream: S,
        tailers: Option<FileTailers>,
        mut console_interaction: Option<ConsoleInteractionStream<'_>>,
    ) -> anyhow::Result<CommandResult> {
        let mut noop_console_interaction = NoopConsoleInteraction;
        let console_interaction: &mut dyn ConsoleInteraction = match &mut console_interaction {
            Some(i) => i as _,
            None => &mut noop_console_interaction as _,
        };

        let mut stream = stream.fuse();
        // TODO(cjhopman): This is fragile. We are handling stdout/stderr here but we also want to stop
        // the streaming of stdout/stderr on some things we see here but importantly we need to finish
        // draining stdout/stderr even if we encounter errors. Also, it looks like we probably drop a lot
        // of events/stdout/stderr if a handle_subscribers() call returns an error.

        // We don't want to return early here without draining stdout/stderr.
        // TODO(brasselsprouts): simpler logic

        let mut tailers = tailers.unwrap_or_else(FileTailers::empty);

        let command_result = loop {
            tokio::select! {
                next = stream.next() => {
                    // Make sure we still flush if next produces an error is accurate
                    match self.handle_stream_next(next).await? {
                        ControlFlow::Continue(()) => {}
                        ControlFlow::Break(res) => break res,
                    }
                }
                Some(event) = tailers.streams.next() => {
                    self.dispatch_tailer_event(event).await?;
                }
                c = console_interaction.char() => {
                    self.handle_console_interaction(c?).await?;
                }
                tick = self.ticker.tick() => {
                    self.tick(&tick).await?;
                }
                else => {
                    unreachable!("The tick branch will always take precedence over an else case.");
                }
            }
        };

        self.flush(&mut Some(tailers)).await?;

        command_result
    }

    /// Given a stream of StreamValues originating from the daemon, "unpacks" it by extracting the command result from the
    /// event stream and returning it.
    /// Also merges all other streams into a single, larger stream
    pub async fn unpack_stream<
        R: TryFrom<command_result::Result, Error = command_result::Result>,
        S: Stream<Item = anyhow::Result<StreamValue>> + Unpin,
    >(
        &mut self,
        stream: S,
        tailers: Option<FileTailers>,
        console_interaction: Option<ConsoleInteractionStream<'_>>,
    ) -> anyhow::Result<CommandOutcome<R>> {
        let command_result = self
            .unpack_stream_inner(stream, tailers, console_interaction)
            .await;

        match command_result {
            Ok(result) => convert_result(result),
            Err(err) => Err(self.handle_error_owned(err).await),
        }
    }

    pub async fn flushing_tailers<R, Fut: Future<Output = R>>(
        &mut self,
        tailers: &mut Option<FileTailers>,
        f: impl FnOnce() -> Fut,
    ) -> anyhow::Result<R> {
        let res = f().await;
        self.flush(tailers).await?;
        Ok(res)
    }

    /// Unpack a single `CommandResult`, log any failures if necessary, and convert it to a
    /// `CommandOutcome`
    pub async fn unpack_oneshot<
        R: TryFrom<command_result::Result, Error = command_result::Result>,
        Fut: Future<Output = Result<tonic::Response<CommandResult>, tonic::Status>>,
    >(
        &mut self,
        tailers: &mut Option<FileTailers>,
        f: impl FnOnce() -> Fut,
    ) -> anyhow::Result<CommandOutcome<R>> {
        let res = self.flushing_tailers(tailers, f).await?;
        // important - do not early return before flushing the buffers!
        let inner = res?.into_inner();
        self.handle_command_result(&inner).await?;

        convert_result(inner)
    }

    /// Helper method to abstract the process of applying an `EventSubscriber` method to all of the subscribers.
    /// Quits on the first error encountered.
    async fn handle_subscribers<'a, Fut>(
        &'a mut self,
        mut f: impl FnMut(&'a mut Box<dyn EventSubscriber>) -> Fut,
    ) -> anyhow::Result<()>
    where
        Fut: Future<Output = anyhow::Result<()>> + 'a,
    {
        for subscriber in &mut self.subscribers {
            f(subscriber).await?;
        }

        Ok(())
    }

    async fn handle_error_owned(&mut self, error: anyhow::Error) -> anyhow::Error {
        let result = self
            .handle_subscribers(|subscriber| subscriber.handle_error(&error))
            .await;
        match result {
            Ok(()) => error,
            Err(e) => EventsCtxError::WrappedStreamError {
                source: error,
                other: e,
            }
            .into(),
        }
    }

    pub async fn flush(&mut self, tailers: &mut Option<FileTailers>) -> anyhow::Result<()> {
        let tailers = match tailers.take() {
            Some(tailers) => tailers,
            None => return Ok(()),
        };
        let mut streams = tailers.stop_reading();

        // We need to loop again to drain stdout/stderr
        let mut complete = false;
        while !complete {
            tokio::select! {
                event = streams.next() => {
                    match event {
                        Some(event) => {self.dispatch_tailer_event(event).await?;}
                        None => {complete = true;}
                    }
                }
                tick = self.ticker.tick() => {
                    self.tick(&tick).await?;
                }
            }
        }

        let tick = self.ticker.tick_now();
        self.tick(&tick).await?;

        Ok(())
    }
}

/// Convert a CommandResult into a CommandOutcome after the CommandResult has been printed by `handle_command_result`.
fn convert_result<R: TryFrom<command_result::Result, Error = command_result::Result>>(
    value: CommandResult,
) -> anyhow::Result<CommandOutcome<R>> {
    match value.result {
        Some(command_result::Result::Error(_)) => Ok(CommandOutcome::Failure(None)),
        Some(value) => match value.try_into() {
            Ok(v) => Ok(CommandOutcome::Success(v)),
            Err(res) => Err(BuckdCommunicationError::UnexpectedResultType(res).into()),
        },
        None => Err(BuckdCommunicationError::EmptyCommandResult.into()),
    }
}

#[async_trait]
impl EventSubscriber for EventsCtx {
    async fn handle_output(&mut self, raw_output: &str) -> anyhow::Result<()> {
        self.handle_subscribers(|subscriber| subscriber.handle_output(raw_output))
            .await
    }

    async fn handle_stderr(&mut self, stderr: &str) -> anyhow::Result<()> {
        self.handle_subscribers(|subscriber| subscriber.handle_stderr(stderr))
            .await
    }

    async fn handle_console_interaction(&mut self, c: char) -> anyhow::Result<()> {
        self.handle_subscribers(|subscriber| subscriber.handle_console_interaction(c))
            .await
    }

    async fn handle_event(&mut self, event: &buck2_events::BuckEvent) -> anyhow::Result<()> {
        self.handle_subscribers(|subscriber| subscriber.handle_event(event))
            .await
    }

    async fn handle_command_result(
        &mut self,
        result: &cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        self.handle_subscribers(|subscriber| subscriber.handle_command_result(result))
            .await
    }

    /// This function is called once per `TICK_SPEED`.
    /// A subscriber will have the opportunity to do an arbitrary process at a reliable interval.
    /// In particular, this is crucial for superconsole so that it can draw itself consistently.
    async fn tick(&mut self, tick: &Tick) -> anyhow::Result<()> {
        self.handle_subscribers(|subscriber| subscriber.tick(tick))
            .await
    }
}

impl FileTailers {
    pub fn new(daemon_dir: &DaemonDir) -> anyhow::Result<Self> {
        let (stdout_tx, stdout) = mpsc::unbounded_channel();
        let (stderr_tx, stderr) = mpsc::unbounded_channel();
        let stdout_tailer = FileTailer::tail_file(daemon_dir.buckd_stdout(), stdout_tx)?;
        let stderr_tailer = FileTailer::tail_file(daemon_dir.buckd_stderr(), stderr_tx)?;
        let this = Self {
            _stdout_tailer: Some(stdout_tailer),
            _stderr_tailer: Some(stderr_tailer),
            streams: FileStreams { stdout, stderr },
        };
        Ok(this)
    }

    pub(crate) fn empty() -> FileTailers {
        FileTailers {
            _stdout_tailer: None,
            _stderr_tailer: None,
            streams: FileStreams {
                // Empty streams.
                stdout: tokio::sync::mpsc::unbounded_channel().1,
                stderr: tokio::sync::mpsc::unbounded_channel().1,
            },
        }
    }

    pub fn stop_reading(self) -> FileStreams {
        // by dropping the tailers, they shut themselves down.
        self.streams
    }
}

#[derive(Error, Debug)]
pub enum EventsCtxError {
    #[error("While propagating error:\n{source:#?}, another error was detected:\n{other:#?}")]
    WrappedStreamError {
        #[source]
        source: anyhow::Error,
        other: anyhow::Error,
    },
}
