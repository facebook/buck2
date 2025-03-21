/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::ControlFlow;
use std::pin::pin;
use std::sync::Arc;
use std::time::SystemTime;

use async_trait::async_trait;
use buck2_cli_proto::command_result;
use buck2_cli_proto::CommandResult;
use buck2_error::BuckErrorContext;
use buck2_event_log::stream_value::StreamValue;
use buck2_events::BuckEvent;
use futures::stream;
use futures::Future;
use futures::FutureExt;
use futures::Stream;
use futures::StreamExt;
use gazebo::prelude::VecExt;

use crate::client_cpu_tracker::ClientCpuTracker;
use crate::command_outcome::CommandOutcome;
use crate::console_interaction_stream::ConsoleInteractionStream;
use crate::console_interaction_stream::NoopSuperConsoleInteraction;
use crate::console_interaction_stream::SuperConsoleInteraction;
use crate::console_interaction_stream::SuperConsoleToggle;
use crate::daemon::client::tonic_status_to_error;
use crate::daemon::client::NoPartialResultHandler;
use crate::exit_result::ExitResult;
use crate::file_tailers::tailers::FileTailers;
use crate::subscribers::subscriber::Tick;
use crate::subscribers::subscribers::EventSubscribers;
use crate::ticker::Ticker;

/// Target number of self.tick() calls per second. These can be used by implementations for regular updates, for example
/// superconsole uses it to re-render the frame and this is what allows it to have constantly updating timers.
/// Other than tick() calls, implementations will only be notified when new events arrive.
const TICKS_PER_SECOND: u32 = 10;

#[derive(Debug, buck2_error::Error)]
#[allow(clippy::large_enum_variant)]
enum BuckdCommunicationError {
    #[error("call to daemon returned an unexpected result type. got `{0:?}`")]
    #[buck2(tag = Tier0)]
    UnexpectedResultType(command_result::Result),
    #[error("buck daemon returned an empty CommandResult")]
    #[buck2(tag = Tier0)]
    EmptyCommandResult,
    #[error("buck daemon request finished without returning a CommandResult")]
    #[buck2(tag = Tier0)]
    MissingCommandResult,
    #[error(
        "The Buck2 daemon was shut down while executing your command. This happened because: {0}"
    )]
    #[buck2(tag = InterruptedByDaemonShutdown)]
    InterruptedByDaemonShutdown(buck2_data::DaemonShutdown),
    #[error("buckd communication encountered an unexpected error `{0:?}`")]
    #[buck2(tag = Tier0)]
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

#[async_trait]
pub trait PartialResultHandler {
    type PartialResult: TryFrom<
            buck2_cli_proto::partial_result::PartialResult,
            Error = buck2_cli_proto::partial_result::PartialResult,
        >;

    async fn handle_partial_result(
        &mut self,
        ctx: PartialResultCtx<'_>,
        partial_res: Self::PartialResult,
    ) -> buck2_error::Result<()>;
}

/// Exposes restricted access to EventsCtx from PartialResultHandler instances.
pub struct PartialResultCtx<'a> {
    inner: &'a mut EventsCtx,
}

impl<'a> PartialResultCtx<'a> {
    pub async fn stdout(&mut self, bytes: &[u8]) -> buck2_error::Result<()> {
        self.inner
            .subscribers
            .for_each_subscriber(|subscriber| subscriber.handle_output(bytes))
            .await
    }
}

/// Manages incoming event streams from the daemon for the buck2 client and
/// forwards them to the appropriate subscribers registered on this struct
pub struct EventsCtx {
    pub(crate) subscribers: EventSubscribers,
    ticker: Ticker,
    client_cpu_tracker: ClientCpuTracker,
}

#[derive(PartialEq, Eq, Debug)]
pub enum FileTailerEvent {
    Stdout(Vec<u8>),
    Stderr(Vec<u8>),
}

impl EventsCtx {
    pub fn new(subscribers: EventSubscribers) -> Self {
        Self {
            subscribers,
            ticker: Ticker::new(TICKS_PER_SECOND),
            client_cpu_tracker: ClientCpuTracker::new(),
        }
    }

    async fn handle_stream_next<Handler>(
        &mut self,
        partial_result_handler: &mut Handler,
        next: Option<Vec<buck2_error::Result<StreamValue>>>,
        shutdown: &mut Option<buck2_data::DaemonShutdown>,
    ) -> buck2_error::Result<ControlFlow<Box<CommandResult>, ()>>
    where
        Handler: PartialResultHandler,
    {
        let next = next.buck_error_context(BuckdCommunicationError::MissingCommandResult)?;
        let mut events = Vec::with_capacity(next.len());
        for next in next {
            let next = match next {
                Ok(next) => next,
                Err(e) => {
                    self.handle_events(events, shutdown).await?;
                    return Err(e).buck_error_context("Buck daemon event bus encountered an error, the root cause (if available) is displayed above this message.");
                }
            };
            match next {
                StreamValue::Event(event) => {
                    let event = event.try_into()?;
                    events.push(event);
                }
                StreamValue::PartialResult(partial_res) => {
                    let partial_res = partial_res
                        .partial_result
                        .buck_error_context("Empty partial result")?
                        .try_into()
                        .map_err(|e| {
                            buck2_error::buck2_error!(
                                buck2_error::ErrorTag::Tier0,
                                "Invalid PartialResult: {:?}",
                                e
                            )
                        })?;
                    partial_result_handler
                        .handle_partial_result(PartialResultCtx { inner: self }, partial_res)
                        .await?;
                }
                StreamValue::Result(res) => {
                    self.handle_events(events, shutdown).await?;
                    self.handle_command_result(&res).await?;
                    return Ok(ControlFlow::Break(res));
                }
            }
        }
        self.handle_events(events, shutdown).await?;
        Ok(ControlFlow::Continue(()))
    }

    async fn dispatch_tailer_event(&mut self, event: FileTailerEvent) -> buck2_error::Result<()> {
        match event {
            FileTailerEvent::Stdout(out) | FileTailerEvent::Stderr(out) => {
                // Sending daemon stdout to stderr.
                // Daemon is not supposed to write anything to stdout.
                // But if daemon does, it should not be used as standard output of buck2.
                self.handle_tailer_stderr(&out).await
            }
        }
    }

    async fn unpack_stream_inner<S, Handler>(
        &mut self,
        partial_result_handler: &mut Handler,
        stream: S,
        tailers: Option<FileTailers>,
        mut console_interaction: Option<ConsoleInteractionStream<'_>>,
    ) -> buck2_error::Result<CommandResult>
    where
        S: Stream<Item = buck2_error::Result<StreamValue>>,
        Handler: PartialResultHandler,
    {
        let mut noop_console_interaction = NoopSuperConsoleInteraction;
        let console_interaction: &mut dyn SuperConsoleInteraction = match &mut console_interaction {
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

        let stream = stream.ready_chunks(1000);
        let mut stream = pin!(stream);

        // NOTE: When unpacking the stream we capture any shutdown event we encounter. If we fail
        // to unpack the stream to completion, we'll use that later.
        let mut shutdown = None;

        let command_result: buck2_error::Result<CommandResult> = try {
            loop {
                tokio::select! {
                    next = stream.next() => {
                        // Make sure we still flush if next produces an error is accurate
                        match self.handle_stream_next(
                            partial_result_handler,
                            next,
                            &mut shutdown
                        ).await? {
                            ControlFlow::Continue(()) => {}
                            ControlFlow::Break(res) => break *res,
                        }
                    }
                    Some(event) = tailers.stream.recv() => {
                        self.dispatch_tailer_event(event).await?;
                    }
                    c = console_interaction.toggle() => {
                        self.handle_console_interaction(&c?).await?;
                    }
                    tick = self.ticker.tick() => {
                        self.tick(&tick).await?;
                    }
                }
            }
        };

        let flush_result = self.flush(Some(tailers)).await;
        let exit_result = self.subscribers.handle_exit().await;

        let command_result = match (command_result, shutdown) {
            (Ok(r), _) => r,
            (Err(e), Some(shutdown)) => {
                // NOTE: When we get an error unpacking the stream, that means we didn't get a
                // CommandResult, or we had an error on the stream. Both of those things are *most
                // often* caused by a daemon shutdown, so if we did have a daemon shutdown reported
                // to us over the event stream, we show that instead. We log the real error for
                // debugging in case that's useful, but it's sufficiently likely (as in: almost
                // certain) the daemon shutdown is the cause for us to simply claim it is.
                tracing::debug!("Original unpack_stream error was: {:#}", e);

                return Err(BuckdCommunicationError::InterruptedByDaemonShutdown(shutdown).into());
            }
            (Err(e), None) => return Err(e),
        };

        flush_result?;
        exit_result?;
        Ok(command_result)
    }

    /// Given a stream of StreamValues originating from the daemon, "unpacks" it by extracting the command result from the
    /// event stream and returning it.
    /// Also merges all other streams into a single, larger stream
    pub async fn unpack_stream<S, Res, Handler>(
        &mut self,
        partial_result_handler: &mut Handler,
        stream: S,
        tailers: Option<FileTailers>,
        console_interaction: Option<ConsoleInteractionStream<'_>>,
    ) -> buck2_error::Result<CommandOutcome<Res>>
    where
        S: Stream<Item = buck2_error::Result<StreamValue>>,
        Res: TryFrom<command_result::Result, Error = command_result::Result>,
        Handler: PartialResultHandler,
    {
        let command_result = self
            .unpack_stream_inner(partial_result_handler, stream, tailers, console_interaction)
            .await;

        match command_result {
            Ok(result) => convert_result(result),
            Err(err) => Err(self.handle_error_owned(err).await),
        }
    }

    /// Unpack a single `CommandResult`, log any failures if necessary, and convert it to a
    /// `CommandOutcome`
    pub async fn unpack_oneshot<
        Res: TryFrom<command_result::Result, Error = command_result::Result>,
        Fut: Future<Output = Result<tonic::Response<CommandResult>, tonic::Status>>,
    >(
        &mut self,
        tailers: Option<FileTailers>,
        f: Fut,
    ) -> buck2_error::Result<CommandOutcome<Res>> {
        let stream = stream::once(f.map(|result| {
            result
                .map(|command_result| StreamValue::Result(Box::new(command_result.into_inner())))
                .map_err(tonic_status_to_error)
        }));
        self.unpack_stream(&mut NoPartialResultHandler, stream, tailers, None)
            .await
    }

    async fn handle_error_owned(&mut self, error: buck2_error::Error) -> buck2_error::Error {
        let error: buck2_error::Error = error.into();
        let result = self
            .subscribers
            .for_each_subscriber(|subscriber| subscriber.handle_error(&error))
            .await;
        match result {
            Ok(()) => error.into(),
            Err(e) => EventsCtxError::WrappedStreamError {
                source: error.into(),
                other: e,
            }
            .into(),
        }
    }

    pub async fn flush(&mut self, tailers: Option<FileTailers>) -> buck2_error::Result<()> {
        let Some(tailers) = tailers else {
            return Ok(());
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

    pub async fn finalize(&mut self) -> Vec<String> {
        self.subscribers.finalize().await
    }
}

/// Convert a CommandResult into a CommandOutcome after the CommandResult has been printed by `handle_command_result`.
fn convert_result<R: TryFrom<command_result::Result, Error = command_result::Result>>(
    value: CommandResult,
) -> buck2_error::Result<CommandOutcome<R>> {
    match value.result {
        Some(command_result::Result::Error(buck2_cli_proto::CommandError { errors })) => {
            Ok(CommandOutcome::Failure(ExitResult::from_errors(&errors)))
        }
        Some(value) => match value.try_into() {
            Ok(v) => Ok(CommandOutcome::Success(v)),
            Err(res) => Err(BuckdCommunicationError::UnexpectedResultType(res).into()),
        },
        None => Err(BuckdCommunicationError::EmptyCommandResult.into()),
    }
}

impl EventsCtx {
    async fn handle_tailer_stderr(&mut self, stderr: &[u8]) -> buck2_error::Result<()> {
        let stderr = String::from_utf8_lossy(stderr);
        let stderr = stderr.trim_end();
        self.subscribers
            .for_each_subscriber(|subscriber| subscriber.handle_tailer_stderr(stderr))
            .await
    }

    async fn handle_console_interaction(
        &mut self,
        toggle: &Option<SuperConsoleToggle>,
    ) -> buck2_error::Result<()> {
        self.subscribers
            .for_each_subscriber(|subscriber| subscriber.handle_console_interaction(toggle))
            .await
    }

    async fn handle_events(
        &mut self,
        events: Vec<BuckEvent>,
        shutdown: &mut Option<buck2_data::DaemonShutdown>,
    ) -> buck2_error::Result<()> {
        let events = events.into_map(|mut event| {
            let timestamp = event.timestamp();
            if let buck2_data::buck_event::Data::Instant(instant_event) = event.data_mut() {
                match &mut instant_event.data {
                    Some(buck2_data::instant_event::Data::Snapshot(snapshot)) => {
                        let now = SystemTime::now();
                        // `None` on overflow.
                        let this_event_client_delay_ms = match now.duration_since(timestamp) {
                            Ok(duration) => i64::try_from(duration.as_millis()).ok(),
                            Err(e) => i64::try_from(e.duration().as_millis())
                                .ok()
                                .and_then(|x| x.checked_neg()),
                        };
                        snapshot.this_event_client_delay_ms = this_event_client_delay_ms;

                        snapshot.client_cpu_percents =
                            self.client_cpu_tracker.tick_cpu_time_percents();
                    }
                    Some(buck2_data::instant_event::Data::DaemonShutdown(msg)) => {
                        *shutdown = Some(msg.clone());
                    }
                    _ => {}
                };
            }
            Arc::new(event)
        });
        self.subscribers
            .for_each_subscriber(|subscriber| subscriber.handle_events(&events))
            .await
    }

    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> buck2_error::Result<()> {
        self.subscribers
            .for_each_subscriber(|subscriber| subscriber.handle_command_result(result))
            .await
    }

    /// This function is called once per `TICK_SPEED`.
    /// A subscriber will have the opportunity to do an arbitrary process at a reliable interval.
    /// In particular, this is crucial for superconsole so that it can draw itself consistently.
    async fn tick(&mut self, tick: &Tick) -> buck2_error::Result<()> {
        self.subscribers
            .for_each_subscriber(|subscriber| subscriber.tick(tick))
            .await
    }
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
pub enum EventsCtxError {
    #[error("While propagating error:\n{source:#?}, another error was detected:\n{other:#?}")]
    WrappedStreamError {
        #[source]
        source: buck2_error::Error,
        other: buck2_error::Error,
    },
}
