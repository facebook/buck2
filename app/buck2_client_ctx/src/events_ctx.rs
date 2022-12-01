/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::ControlFlow;
use std::sync::Arc;
use std::time::SystemTime;

use anyhow::Context;
use buck2_common::daemon_dir::DaemonDir;
use buck2_events::BuckEvent;
use cli_proto::command_result;
use cli_proto::CommandResult;
use futures::stream::FuturesUnordered;
use futures::Future;
use futures::Stream;
use futures::StreamExt;
use gazebo::prelude::VecExt;
use thiserror::Error;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedReceiver;

use crate::client_cpu_tracker::ClientCpuTracker;
use crate::command_outcome::CommandOutcome;
use crate::console_interaction_stream::ConsoleInteraction;
use crate::console_interaction_stream::ConsoleInteractionStream;
use crate::console_interaction_stream::NoopConsoleInteraction;
use crate::file_tailer::FileTailer;
use crate::file_tailer::StdoutOrStderr;
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
pub struct EventsCtx {
    pub daemon_dir: DaemonDir,
    pub(crate) subscribers: Vec<Box<dyn EventSubscriber>>,
    ticker: Ticker,
    client_cpu_tracker: ClientCpuTracker,
}

#[derive(PartialEq, Eq, Debug)]
pub enum FileTailerEvent {
    Stdout(String),
    Stderr(String),
}

pub struct FileTailers {
    _stdout_tailer: Option<FileTailer>,
    _stderr_tailer: Option<FileTailer>,
    stream: UnboundedReceiver<FileTailerEvent>,
}

impl EventsCtx {
    pub(crate) fn new(daemon_dir: DaemonDir, subscribers: Vec<Box<dyn EventSubscriber>>) -> Self {
        Self {
            daemon_dir,
            subscribers,
            ticker: Ticker::new(TICKS_PER_SECOND),
            client_cpu_tracker: ClientCpuTracker::new(),
        }
    }

    async fn handle_stream_next(
        &mut self,
        next: Option<Vec<anyhow::Result<StreamValue>>>,
    ) -> anyhow::Result<ControlFlow<CommandResult, ()>> {
        let next = next.context(BuckdCommunicationError::MissingCommandResult)?;
        let mut events = Vec::with_capacity(next.len());
        for next in next {
            let next = match next {
                Ok(next) => next,
                Err(e) => {
                    self.handle_events(events).await?;
                    return Err(e).context("Buck daemon event bus encountered an error");
                }
            };
            match next {
                StreamValue::Event(event) => {
                    let event = event.try_into()?;
                    events.push(event);
                }
                StreamValue::Result(res) => {
                    self.handle_events(events).await?;
                    self.handle_command_result(&res).await?;
                    return Ok(ControlFlow::Break(res));
                }
            }
        }
        self.handle_events(events).await?;
        Ok(ControlFlow::Continue(()))
    }

    async fn dispatch_tailer_event(&mut self, event: FileTailerEvent) -> anyhow::Result<()> {
        match event {
            FileTailerEvent::Stdout(stdout) => self.handle_tailer_stdout(&stdout).await,
            FileTailerEvent::Stderr(line) => self.handle_tailer_stderr(line.trim_end()).await,
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

        // TODO(cjhopman): This is fragile. We are handling stdout/stderr here but we also want to stop
        // the streaming of stdout/stderr on some things we see here but importantly we need to finish
        // draining stdout/stderr even if we encounter errors. Also, it looks like we probably drop a lot
        // of events/stdout/stderr if a handle_subscribers() call returns an error.

        // We don't want to return early here without draining stdout/stderr.
        // TODO(brasselsprouts): simpler logic

        let mut tailers = tailers.unwrap_or_else(FileTailers::empty);

        let mut stream = stream.ready_chunks(1000);

        let command_result: anyhow::Result<CommandResult> = try {
            loop {
                tokio::select! {
                    next = stream.next() => {
                        // Make sure we still flush if next produces an error is accurate
                        match self.handle_stream_next(next).await? {
                            ControlFlow::Continue(()) => {}
                            ControlFlow::Break(res) => break res,
                        }
                    }
                    Some(event) = tailers.stream.recv() => {
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
            }
        };

        let flush_result = self.flush(&mut Some(tailers)).await;
        let exit_result = self.handle_exit().await;

        let command_result = command_result?;
        flush_result?;
        exit_result?;
        Ok(command_result)
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
        f: impl FnMut(&'a mut Box<dyn EventSubscriber>) -> Fut,
    ) -> anyhow::Result<()>
    where
        Fut: Future<Output = anyhow::Result<()>> + 'a,
    {
        let mut futures: FuturesUnordered<_> = self.subscribers.iter_mut().map(f).collect();
        while let Some(res) = futures.next().await {
            res?;
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
                event = streams.recv() => {
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

impl EventsCtx {
    async fn handle_tailer_stdout(&mut self, raw_output: &str) -> anyhow::Result<()> {
        self.handle_subscribers(|subscriber| subscriber.handle_tailer_stdout(raw_output))
            .await
    }

    async fn handle_tailer_stderr(&mut self, stderr: &str) -> anyhow::Result<()> {
        self.handle_subscribers(|subscriber| subscriber.handle_tailer_stderr(stderr))
            .await
    }

    async fn handle_console_interaction(&mut self, c: char) -> anyhow::Result<()> {
        self.handle_subscribers(|subscriber| subscriber.handle_console_interaction(c))
            .await
    }

    async fn handle_events(&mut self, events: Vec<BuckEvent>) -> anyhow::Result<()> {
        let events = events.into_map(|mut event| {
            let timestamp = event.timestamp();
            if let buck2_data::buck_event::Data::Instant(instant_event) = event.data_mut() {
                if let Some(buck2_data::instant_event::Data::Snapshot(snapshot)) =
                    &mut instant_event.data
                {
                    let now = SystemTime::now();
                    // `None` on overflow.
                    let this_event_client_delay_ms = match now.duration_since(timestamp) {
                        Ok(duration) => i64::try_from(duration.as_millis()).ok(),
                        Err(e) => i64::try_from(e.duration().as_millis())
                            .ok()
                            .and_then(|x| x.checked_neg()),
                    };
                    snapshot.this_event_client_delay_ms = this_event_client_delay_ms;

                    snapshot.client_cpu_percents = self.client_cpu_tracker.tick_cpu_time_percents();
                }
            }
            Arc::new(event)
        });
        self.handle_subscribers(|subscriber| subscriber.handle_events(&events))
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

    async fn handle_exit(&mut self) -> anyhow::Result<()> {
        let mut r = Ok(());
        for subscriber in &mut self.subscribers {
            // Exit all subscribers, do not stop on first one.
            let subscriber_err = subscriber.exit().await;
            if r.is_ok() {
                // Keep first error.
                r = subscriber_err;
            }
        }
        r
    }
}

impl FileTailers {
    pub fn new(daemon_dir: &DaemonDir) -> anyhow::Result<Self> {
        let (tx, rx) = mpsc::unbounded_channel();
        let stdout_tailer = FileTailer::tail_file(
            daemon_dir.buckd_stdout(),
            tx.clone(),
            StdoutOrStderr::Stdout,
        )?;
        let stderr_tailer =
            FileTailer::tail_file(daemon_dir.buckd_stderr(), tx, StdoutOrStderr::Stderr)?;
        let this = Self {
            _stdout_tailer: Some(stdout_tailer),
            _stderr_tailer: Some(stderr_tailer),
            stream: rx,
        };
        Ok(this)
    }

    pub fn empty() -> FileTailers {
        FileTailers {
            _stdout_tailer: None,
            _stderr_tailer: None,
            // Empty stream.
            stream: mpsc::unbounded_channel().1,
        }
    }

    pub fn stop_reading(self) -> UnboundedReceiver<FileTailerEvent> {
        // by dropping the tailers, they shut themselves down.
        self.stream
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
