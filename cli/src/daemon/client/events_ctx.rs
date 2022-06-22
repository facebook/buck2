/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::TryFrom;
use std::convert::TryInto;
use std::path::Path;

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::fs::paths::AbsPathBuf;
use cli_proto::command_result;
use cli_proto::CommandResult;
use events::subscriber::EventSubscriber;
use events::subscriber::Tick;
use futures::Future;
use futures::Stream;
use futures::StreamExt;
use thiserror::Error;
use tokio::time;
use tokio::time::Duration;
use tokio::time::Instant;
use tokio::time::Interval;
use tokio::time::MissedTickBehavior;
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::daemon::client::file_tailer::FileTailer;
use crate::daemon::client::BuckdCommunicationError;
use crate::daemon::client::CommandOutcome;
use crate::daemon::client::StreamValue;

/// Target number of self.tick() calls per second. These can be used by implementations for regular updates, for example
/// superconsole uses it to re-render the frame and this is what allows it to have constantly updating timers.
/// Other than tick() calls, implementations will only be notified when new events arrive.
const TICKS_PER_SECOND: u32 = 10;

/// Manages incoming event streams from the daemon for the buck2 client and
/// fowards them to the appropriate subscribers registered on this struct
pub(crate) struct EventsCtx {
    pub(crate) daemon_dir: AbsPathBuf,
    pub(crate) subscribers: Vec<Box<dyn EventSubscriber>>,
    ticker: Ticker,
}

pub(crate) struct FileStreams {
    stdout: UnboundedReceiverStream<String>,
    stderr: UnboundedReceiverStream<String>,
}

pub(crate) struct FileTailers {
    _stdout_tailer: FileTailer,
    _stderr_tailer: FileTailer,
    streams: FileStreams,
}

/// A simple wrapper around a [Interval] that tracks information about start/elapsed time and tick numbers. Note
/// that ticks are not necessarily sequential, some may be skipped (and this indicates that ticks are running
/// slower than requested).
struct Ticker {
    ticks_per_second: u32,
    interval_duration: Duration,
    interval: Interval,
    start_time: Instant,
    previous_tick: u32,
}

impl Ticker {
    fn new(ticks_per_second: u32) -> Self {
        let interval_duration = Duration::from_secs_f64(1.0 / (ticks_per_second as f64));
        let mut interval = time::interval(interval_duration);
        interval.set_missed_tick_behavior(MissedTickBehavior::Skip);
        Self {
            interval_duration,
            interval,
            ticks_per_second,
            start_time: Instant::now(),
            previous_tick: 0,
        }
    }

    async fn tick(&mut self) -> Tick {
        let current = self.interval.tick().await;
        self.tick_at(current)
    }

    fn tick_now(&mut self) -> Tick {
        self.tick_at(Instant::now())
    }

    fn tick_at(&mut self, current: Instant) -> Tick {
        // For time::interval, the Instant is the target instant for that tick and so it's possible
        // on the first one for it to actually be ealier than our start time.
        let elapsed_time = current
            .checked_duration_since(self.start_time)
            .unwrap_or(Duration::ZERO);

        // 0 is used to represent that we haven't seen any ticks, so if we tick immediately after starting we want that tick to be 1 (that's why we offset by 1 here).
        let tick_count_f64 =
            1.0 + (elapsed_time.as_secs_f64() / self.interval_duration.as_secs_f64()).round();
        let current_tick = tick_count_f64.min(std::u32::MAX as f64).round() as u32;

        let previous_tick = self.previous_tick;
        self.previous_tick = current_tick;

        Tick {
            previous_tick,
            current_tick,
            start_time: self.start_time.into_std(),
            ticks_per_second: self.ticks_per_second,
            elapsed_time,
        }
    }
}

impl EventsCtx {
    pub(crate) fn new(daemon_dir: AbsPathBuf, subscribers: Vec<Box<dyn EventSubscriber>>) -> Self {
        Self {
            daemon_dir,
            subscribers,
            ticker: Ticker::new(TICKS_PER_SECOND),
        }
    }

    /// Given a stream of StreamValues originating from the daemon, "unpacks" it by extracting the command result from the
    /// event stream and returning it.
    /// Also merges all other streams into a single, larger stream
    pub(crate) async fn unpack_stream<
        R: TryFrom<command_result::Result, Error = command_result::Result>,
        S: Stream<Item = anyhow::Result<StreamValue>> + Unpin,
    >(
        &mut self,
        stream: S,
        tailers: &mut Option<FileTailers>,
    ) -> anyhow::Result<CommandOutcome<R>> {
        let command_result: anyhow::Result<CommandResult> = try {
            let mut stream = stream.fuse();
            // TODO(cjhopman): This is fragile. We are handling stdout/stderr here but we also want to stop
            // the streaming of stdout/stderr on some things we see here but importantly we need to finish
            // draining stdout/stderr even if we encounter errors. Also, it looks like we probably drop a lot
            // of events/stdout/stderr if a handle_subscribers() call returns an error.

            // We don't want to return early here without draining stdout/stderr.
            // TODO(brasselsprouts): simpler logic
            let command_result = match tailers.take() {
                Some(mut tailers) => {
                    let command_result = loop {
                        tokio::select! {
                            next = stream.next() => {
                                // Make sure we still flush if next produces an error is accurate
                                let next: anyhow::Result<_> = try {
                                    next.context(BuckdCommunicationError::MissingCommandResult)?
                                        .context("Buck daemon event bus encountered an error")?
                                };
                                match next {
                                    Ok(StreamValue::Event(event)) => {
                                        let event = event.try_into()?;
                                        self.handle_event(&event).await?;
                                    }
                                    Ok(StreamValue::Result(res)) => {
                                        self.handle_command_result(&res).await?;
                                        break Ok(res)
                                    }
                                    Err(e) => {break Err(e)}
                                };
                            }
                            Some(stdout) = tailers.streams.stdout.next() => {
                                self.handle_output(&stdout).await?;
                            }
                            Some(stderr) = tailers.streams.stderr.next() => {
                                self.handle_stderr(stderr.trim_end()).await?;
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

                    command_result?
                }
                None => loop {
                    tokio::select! {
                        next = stream.next() => {
                            let next = next
                                .context(BuckdCommunicationError::MissingCommandResult)?
                                .context("Buck daemon event bus encountered an error")?;
                            match next {
                                StreamValue::Event(event) => {
                                    let event = event.try_into()?;
                                    self.handle_event(&event).await?;
                                }
                                StreamValue::Result(res) => {
                                    self.handle_command_result(&res).await?;
                                    break res
                                }
                            };
                        }
                        tick = self.ticker.tick() => {
                            self.tick(&tick).await?;
                        }
                        else => {
                            unreachable!("The tick branch will always take precedence over an else case.");
                        }
                    }
                },
            };

            command_result
        };

        match command_result {
            Ok(result) => convert_result(result),
            Err(err) => Err(self.handle_error_owned(err).await),
        }
    }

    pub(crate) async fn flushing_tailers<R, Fut: Future<Output = R>>(
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
    pub(crate) async fn unpack_oneshot<
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

    pub(crate) async fn flush(&mut self, tailers: &mut Option<FileTailers>) -> anyhow::Result<()> {
        let tailers = match tailers.take() {
            Some(tailers) => tailers,
            None => return Ok(()),
        };
        let mut streams = tailers.stop_reading();

        // We need to loop again to drain stdout/stderr
        let mut stdout_complete = false;
        let mut stderr_complete = false;
        while !stdout_complete || !stderr_complete {
            tokio::select! {
                stdout = streams.stdout.next(), if !stdout_complete => {
                    match stdout {
                        Some(stdout) => {self.handle_output(&stdout).await?;}
                        None => {stdout_complete = true;}
                    }
                }
                stderr = streams.stderr.next(), if !stderr_complete => {
                    match stderr {
                        Some(stderr) => {self.handle_stderr(stderr.trim_end()).await?;}
                        None => {stderr_complete = true;}
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

/// Convert a CommandResult into a CommandOutcome after the CommandResult has been printed elsewhere.
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

    async fn handle_event(&mut self, event: &events::BuckEvent) -> anyhow::Result<()> {
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
    pub(crate) fn new(daemon_dir: &Path) -> anyhow::Result<Self> {
        let (stdout, stdout_tailer) = FileTailer::tail_file(daemon_dir.join("buckd.stdout"))?;
        let (stderr, stderr_tailer) = FileTailer::tail_file(daemon_dir.join("buckd.stderr"))?;
        let this = Self {
            _stdout_tailer: stdout_tailer,
            _stderr_tailer: stderr_tailer,
            streams: FileStreams { stdout, stderr },
        };
        Ok(this)
    }

    pub(crate) fn stop_reading(self) -> FileStreams {
        // by dropping the tailers, they shut themselves down.
        self.streams
    }
}

#[derive(Error, Debug)]
pub(crate) enum EventsCtxError {
    #[error("While propagating error:\n{source:#?}, another error was detected:\n{other:#?}")]
    WrappedStreamError {
        #[source]
        source: anyhow::Error,
        other: anyhow::Error,
    },
}
