/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

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
use buck2_event_log::read::EventLogPathBuf;
use buck2_event_log::stream_value::StreamValue;
use buck2_event_log::utils::Invocation;
use futures::Stream;
use futures::StreamExt;
use futures::TryStreamExt;
use tokio::pin;
use tokio::sync::mpsc::UnboundedSender;
use tokio::time::Instant;
use tokio::time::Sleep;
use tokio_stream::wrappers::UnboundedReceiverStream;

/// Replay an event log.
///
/// This command allows visualizing an existing event log in a Superconsole.
#[derive(Debug, clap::Parser)]
#[clap(trailing_var_arg = true)]
pub struct ReplayCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,

    #[clap(
        long,
        help = "Control the playback speed using a float (i.e. 0.5, 2, etc)",
        value_name = "NUMBER"
    )]
    pub speed: Option<f64>,

    /// Preload the event log. This is typically only useful for benchmarking.
    #[clap(long)]
    preload: bool,

    #[clap(help = "Override the arguments")]
    pub override_args: Vec<String>,

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
            preload,
            console_opts,
            override_args: _,
        } = self;
        let work = async {
            let (event_stream, invocation) =
                make_replayer(event_log.get(&ctx).await?, speed, preload).await?;
            let console = get_console_with_root(
                invocation.trace_id,
                console_opts.console_type,
                ctx.verbosity,
                true,
                speed,
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

pub async fn make_replayer(
    log_path: EventLogPathBuf,
    speed: Option<f64>,
    preload: bool,
) -> buck2_error::Result<(
    impl Stream<Item = buck2_error::Result<StreamValue>> + Unpin,
    Invocation,
)> {
    let (invocation, events) = log_path.unpack_stream().await?;

    let events = if preload {
        let events = events.try_collect::<Vec<_>>().await?;
        futures::stream::iter(events).map(Ok).left_stream()
    } else {
        events.right_stream()
    };

    let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
    tokio::task::spawn(replay_events_into(sender, events, speed));

    Ok((UnboundedReceiverStream::new(receiver), invocation))
}

/// Replays the events into the sink, but inserts an appropriate delay between events
async fn replay_events_into(
    sink: UnboundedSender<buck2_error::Result<StreamValue>>,
    events: impl Stream<Item = buck2_error::Result<StreamValue>>,
    speed: Option<f64>,
) {
    let mut syncher = Syncher::new(speed);

    pin!(events);
    let Some((mut next_event, mut next_sleep)) =
        find_next_event_with_delay(&sink, &mut syncher, &mut events).await
    else {
        return;
    };
    loop {
        next_sleep.await;
        if sink.send(next_event).is_err() {
            // The sink is closed, so we can stop sending events.
            return;
        }
        match find_next_event_with_delay(&sink, &mut syncher, &mut events).await {
            Some((event, sleep)) => {
                next_event = event;
                next_sleep = sleep;
            }
            None => break,
        }
    }
}

/// Replay events from the stream into the sink until we find the first event that requires a delay
async fn find_next_event_with_delay(
    sink: &UnboundedSender<buck2_error::Result<StreamValue>>,
    syncher: &mut Syncher,
    events: &mut (impl Stream<Item = buck2_error::Result<StreamValue>> + Unpin),
) -> Option<(buck2_error::Result<StreamValue>, Sleep)> {
    while let Some(mut event) = events.next().await {
        match &event {
            Ok(StreamValue::Event(buck_event)) => {
                match syncher.synch_playback_time(&buck_event) {
                    Ok(delay) => {
                        return Some((event, delay));
                    }
                    Err(e) => {
                        // We couldn't process this event, send an error instead
                        event = Err(e);
                    }
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

/// Handle time drifting when replaying events - add pauses in between events to simulate the real deal.
struct Syncher {
    start: Option<(Instant, SystemTime)>,
    speed: f64,
}

impl Syncher {
    fn new(playback_speed: Option<f64>) -> Self {
        Self {
            start: None,
            speed: playback_speed.unwrap_or(1.0),
        }
    }

    /// Returns an appropriate delay for this event.
    ///
    /// The first event will be sent immediately. Each subsequent event will be sent with a delay
    /// based on its time since that first event.
    fn synch_playback_time(&mut self, event: &buck2_data::BuckEvent) -> buck2_error::Result<Sleep> {
        let event_time = SystemTime::try_from(event.timestamp.unwrap())?;
        let (sync_start, log_start) = self.start.get_or_insert((Instant::now(), event_time));
        let log_offset_time = event_time.duration_since(*log_start)?;
        let sync_offset_time = log_offset_time.div_f64(self.speed);
        let sync_event_time = *sync_start + sync_offset_time;
        Ok(tokio::time::sleep_until(sync_event_time))
    }
}
