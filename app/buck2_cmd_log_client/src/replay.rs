/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cmp::Ordering;
use std::future::pending;
use std::time::Duration;
use std::time::SystemTime;

use buck2_client_ctx::client_ctx::BuckSubcommand;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::ui::get_console_with_root;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::event_log_options::EventLogOptions;
use buck2_client_ctx::events_ctx::DaemonEventsCtx;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::signal_handler::with_simple_sigint_handler;
use buck2_client_ctx::subscribers::superconsole::timekeeper::Clock;
use buck2_client_ctx::subscribers::superconsole::timekeeper::Timekeeper;
use buck2_client_ctx::subscribers::superconsole::timekeeper::duration_between_timestamps;
use buck2_client_ctx::ticker::Tick;
use buck2_event_log::read::EventLogPathBuf;
use buck2_event_log::stream_value::StreamValue;
use buck2_event_log::utils::Invocation;
use buck2_event_observer::span_tracker::EventTimestamp;
use futures::Stream;
use futures::StreamExt;
use futures::TryStreamExt;
use futures::future::Either;
use tokio::pin;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::oneshot;
use tokio::time::Instant;
use tokio_stream::wrappers::UnboundedReceiverStream;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub(crate) enum ReplayError {
    #[error("Invalid speed {0}")]
    InvalidSpeed(f64),
    #[error("Invalid seek {0}")]
    InvalidSeek(f64),
}

/// Replay an event log.
///
/// This command allows visualizing an existing event log in a Superconsole.
#[derive(Debug, clap::Parser)]
pub struct ReplayCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,

    #[clap(
        long,
        help = "Control the playback speed using a float (i.e. 0.5, 2, etc)",
        value_name = "NUMBER",
        default_value = "1.0"
    )]
    pub speed: f64,

    /// Skip to the given number of seconds after the start of the command before starting the
    /// replay
    #[clap(long, default_value = "0.0")]
    pub seek: f64,

    /// Preload the event log. This is typically only useful for benchmarking.
    #[clap(long)]
    preload: bool,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,
}

impl BuckSubcommand for ReplayCommand {
    const COMMAND_NAME: &'static str = "log-replay";

    async fn exec_impl(
        self,
        _matches: BuckArgMatches<'_>,
        mut ctx: ClientCommandContext<'_>,
        _events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let Self {
            event_log,
            speed,
            seek,
            preload,
            console_opts,
        } = self;

        if !speed.is_finite() || speed <= 0.0 {
            return ExitResult::from(buck2_error::Error::from(ReplayError::InvalidSpeed(speed)));
        }

        if !seek.is_finite() || seek < 0.0 {
            return ExitResult::from(buck2_error::Error::from(ReplayError::InvalidSeek(seek)));
        }

        let seek = Duration::from_secs(1).mul_f64(seek);

        let work = async {
            let (event_stream, invocation, timekeeper) =
                make_replayer(event_log.get(&ctx).await?, speed, seek, preload).await?;
            let console = get_console_with_root(
                invocation.trace_id,
                console_opts.console_type,
                ctx.verbosity,
                true,
                timekeeper,
                "(replay)", // Could be better
                console_opts.superconsole_config(),
                None,
            );

            let mut events_ctx = EventsCtx::new(None, vec![console]);
            let res = DaemonEventsCtx::without_tailers(&mut events_ctx)
                .unpack_stream::<_, ReplayResult, _>(
                    &mut NoPartialResultHandler,
                    event_stream,
                    ctx.console_interaction_stream(&console_opts),
                )
                .await;

            if let Err(e) = &res {
                let msg = "request finished without returning a CommandResult";
                if e.to_string().contains(msg) {
                    buck2_client_ctx::eprintln!(
                        "Warning: Incomplete log. Replay may be inaccurate."
                    )?;
                };
            };

            let res = res??;
            for e in &res.errors {
                buck2_client_ctx::eprintln!("{}", e.message)?;
            }

            // FIXME(JakobDegen)(easy): This should probably return failures if there were errors
            ExitResult::success()
        };

        with_simple_sigint_handler(work)
            .await
            .unwrap_or_else(ExitResult::signal_interrupt)
    }
}

struct ReplayResult {
    errors: Vec<buck2_data::ErrorReport>,
}

impl TryFrom<buck2_cli_proto::command_result::Result> for ReplayResult {
    type Error = buck2_cli_proto::command_result::Result;

    fn try_from(v: buck2_cli_proto::command_result::Result) -> Result<Self, Self::Error> {
        use buck2_cli_proto::command_result::Result;

        // It would be good to declare this as a extension trait on our types, but for now to
        // support Replay this is fine;
        let errors = match v {
            Result::Error(error) => vec![error],
            Result::BuildResponse(v) => v.errors,
            Result::TestResponse(v) => v.errors,
            Result::BxlResponse(v) => v.errors,
            _ => Vec::new(),
        };

        Ok(Self { errors })
    }
}

async fn make_replayer(
    log_path: EventLogPathBuf,
    speed: f64,
    seek: Duration,
    preload: bool,
) -> buck2_error::Result<(
    impl Stream<Item = buck2_error::Result<StreamValue>> + Unpin,
    Invocation,
    Timekeeper,
)> {
    let (invocation, events) = log_path.unpack_stream().await?;

    let mut events = if preload {
        let events = events.try_collect::<Vec<_>>().await?;
        futures::stream::iter(events).map(Ok).left_stream()
    } else {
        events.right_stream()
    };

    let (sink, receiver) = tokio::sync::mpsc::unbounded_channel();

    let start_time = if let Some(start_time) = invocation.start_time {
        start_time.into()
    } else {
        // We want to support old log formats without the invocation-level start time, so use the
        // time of the first event
        match find_next_event_with_delay(&sink, &mut events, None).await {
            Some((e, ts)) => {
                // If the other side has shut down, ignore it for now, we'll notice immediately after
                drop(sink.send(e));
                ts
            }
            None => {
                // The event log didn't contain a single timestamp. Making one up at this point
                // seems fine
                SystemTime::UNIX_EPOCH.into()
            }
        }
    };

    let seek_timestamp = timestamp_add_duration(start_time, seek);

    // Note: Seeking forward in time on a large log might take a while; intentionally do this before
    // computing the `command_start_instant` so that the time that superconsole starts up actually
    // aligns with that instant and not that instant + however long this seek took
    let res = find_next_event_with_delay(&sink, &mut events, Some(seek_timestamp)).await;

    // The point in real time at which we treat the command as having happened - delays of
    // subsequent events are calculated relative to this.
    let command_start_instant = Instant::now();

    let syncher = Syncher {
        reference_instant: command_start_instant,
        reference_timestamp: seek_timestamp,
        speed,
    };

    // Buffer is 1 because we only have one sender anyway
    let (speed_update_request_sender, speed_update_requests) = mpsc::channel(1);

    if let Some((first_event, first_event_timestamp)) = res {
        tokio::task::spawn(replay_events_into(
            sink,
            events,
            syncher,
            speed_update_requests,
            first_event,
            first_event_timestamp,
        ));
    };

    let timekeeper = Timekeeper::new(
        Box::new(ReplayClock {
            syncher,
            speed_update_request_sender,
            pause_state: None,
        }),
        EventTimestamp(start_time),
    );

    Ok((
        UnboundedReceiverStream::new(receiver),
        invocation,
        timekeeper,
    ))
}

/// Replays the events into the sink, but inserts an appropriate delay between events
async fn replay_events_into(
    sink: UnboundedSender<buck2_error::Result<StreamValue>>,
    events: impl Stream<Item = buck2_error::Result<StreamValue>>,
    syncher: Syncher,
    mut speed_update_requests: mpsc::Receiver<SpeedUpdateRequest>,
    first_event: buck2_error::Result<StreamValue>,
    first_event_timestamp: prost_types::Timestamp,
) {
    pin!(events);

    let mut syncher = syncher;

    let mut next_event = first_event;
    let mut next_event_timestamp = first_event_timestamp;

    loop {
        tokio::select! {
            _ = syncher.convert_timestamp(next_event_timestamp) => {
                if sink.send(next_event).is_err() {
                    // The sink is closed, so we can stop sending events.
                    return;
                }
                match find_next_event_with_delay(&sink, &mut events, None).await {
                    Some((event, event_timestamp)) => {
                        next_event = event;
                        next_event_timestamp = event_timestamp;
                    }
                    None => break,
                }
            }
            Some(req) = speed_update_requests.recv() => {
                // When changing the speed, we need to reflect that the speed is only being changed
                // *at the current time* and not retroactively for the whole replay. So we implement
                // speed changing by setting the reference instant to now, which means that any
                // future action that uses the speed to scale a time only scales the interval
                // between then and now
                let new_reference_instant = Instant::now();
                let new_syncher = Syncher {
                    reference_instant: new_reference_instant,
                    reference_timestamp: syncher.convert_instant(new_reference_instant),
                    speed: req.new_speed,
                };
                // This can plausibly happen right at the end of a command, though see the note in
                // the clock about this maybe being non-ideal
                _ = req.ret.send(new_syncher);
                // This works because, as a part of the select loop, if we entered this branch we
                // cancelled the above "sleep until we should send the next event." In the next
                // iteration of the loop, the duration of this sleep will be recalculated using the
                // new syncher (and hence new speed)
                syncher = new_syncher;
            }
        }
    }
}

/// Replay events from the stream into the sink until we find the first event that requires a delay
async fn find_next_event_with_delay(
    sink: &UnboundedSender<buck2_error::Result<StreamValue>>,
    events: &mut (impl Stream<Item = buck2_error::Result<StreamValue>> + Unpin),
    min_timestamp: Option<prost_types::Timestamp>,
) -> Option<(buck2_error::Result<StreamValue>, prost_types::Timestamp)> {
    while let Some(event) = events.next().await {
        match &event {
            Ok(StreamValue::Event(buck_event)) => {
                let ts = buck_event.timestamp.unwrap();
                if min_timestamp
                    .is_none_or(|min_timestamp| cmp_timestamps(min_timestamp, ts).is_le())
                {
                    return Some((event, ts));
                }
            }
            // Most other kinds of events don't really happen, don't need a delay for them
            _ => {}
        }
        if sink.send(event).is_err() {
            // The sink is closed, so we can stop sending events.
            return None;
        }
    }
    None
}

/// State that describes how to convert an instant in the timeline of the replay command to a
/// timestamp in the timeline of the command being replayed
#[derive(Copy, Clone)]
struct Syncher {
    reference_instant: Instant,
    reference_timestamp: prost_types::Timestamp,
    speed: f64,
}

impl Syncher {
    fn convert_instant(&self, instant: Instant) -> prost_types::Timestamp {
        let elapsed = instant
            .saturating_duration_since(self.reference_instant)
            .mul_f64(self.speed);
        timestamp_add_duration(self.reference_timestamp, elapsed)
    }

    /// Given a timestamp, converts it to an instant in the timeline of the replay command and
    /// returns a future that sleeps until that instant is reached
    fn convert_timestamp(
        &mut self,
        event_time: prost_types::Timestamp,
    ) -> impl Future<Output = ()> {
        if self.speed == 0.0 {
            // Avoid the division by zero below
            return Either::Left(pending());
        }
        let log_offset_time = duration_between_timestamps(self.reference_timestamp, event_time);
        let sync_offset_time = log_offset_time.div_f64(self.speed);
        Either::Right(tokio::time::sleep_until(
            self.reference_instant + sync_offset_time,
        ))
    }
}

struct ReplayClock {
    syncher: Syncher,
    speed_update_request_sender: mpsc::Sender<SpeedUpdateRequest>,
    /// If `Some`, the replay is paused and the contained value is the last speed
    pause_state: Option<f64>,
}

#[async_trait::async_trait]
impl Clock for ReplayClock {
    fn event_timestamp_for_tick(&mut self, tick: Tick) -> EventTimestamp {
        EventTimestamp(self.syncher.convert_instant(tick.current_monotonic))
    }

    async fn scale_speed(&mut self, factor: f64) -> Option<String> {
        if let Some(pause_state) = &mut self.pause_state {
            *pause_state *= factor;
        } else {
            self.set_speed(self.syncher.speed * factor).await;
        }
        None
    }

    async fn toggle_pause(&mut self) -> Option<String> {
        match self.pause_state {
            Some(speed) => {
                self.pause_state = None;
                self.set_speed(speed).await;
            }
            None => {
                self.pause_state = Some(self.syncher.speed);
                self.set_speed(0.0).await;
            }
        }
        None
    }
}

impl ReplayClock {
    async fn set_speed(&mut self, new_speed: f64) {
        let (ret, recv) = oneshot::channel();
        let Ok(()) = self
            .speed_update_request_sender
            .send(SpeedUpdateRequest { new_speed, ret })
            // Won't ever actually block
            .await
        else {
            // This might happen right at the end of a command with the right timing, seems fine to ignore
            return;
        };
        // Unfortunately, this isn't cancellation-safe - if this future gets cancelled at this await
        // point, we'll have updated the syncher in the replayer but not here.
        //
        // Probably that's fine in practice, it's not clear if we'd ever actually expect this to get
        // cancelled short of the entire client shutting down, but it's a bit of a shame
        let Ok(new_syncher) = recv.await else {
            // Again, might happen at the end of a command
            return;
        };
        self.syncher = new_syncher;
    }
}

/// A message sent from the clock to the replayer requesting a change to the speed
struct SpeedUpdateRequest {
    new_speed: f64,
    /// A oneshot on which to return a new syncher representing the updated state
    ret: oneshot::Sender<Syncher>,
}

fn timestamp_add_duration(
    timestamp: prost_types::Timestamp,
    duration: Duration,
) -> prost_types::Timestamp {
    let all_nanos = (timestamp.nanos as u64) + duration.subsec_nanos() as u64;
    let nanos = all_nanos % 1_000_000_000;
    let seconds = (timestamp.seconds as u64) + duration.as_secs() + all_nanos / 1_000_000_000;
    prost_types::Timestamp {
        seconds: seconds as i64,
        nanos: nanos as i32,
    }
}

fn cmp_timestamps(first: prost_types::Timestamp, second: prost_types::Timestamp) -> Ordering {
    Ord::cmp(&first.seconds, &second.seconds).then_with(|| Ord::cmp(&first.nanos, &second.nanos))
}
