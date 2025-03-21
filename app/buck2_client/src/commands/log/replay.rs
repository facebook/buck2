/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::replayer::Replayer;
use buck2_client_ctx::signal_handler::with_simple_sigint_handler;
use buck2_client_ctx::subscribers::get::get_console_with_root;
use buck2_client_ctx::subscribers::subscribers::EventSubscribers;
use buck2_error::buck2_error;

use crate::commands::log::options::EventLogOptions;

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

impl ReplayCommand {
    pub fn exec(self, _matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        let Self {
            event_log,
            speed,
            preload,
            console_opts,
            override_args: _,
        } = self;

        ctx.instant_command_no_log("log-replay", |mut ctx| async move {
            let work = async {
                let (replayer, invocation) =
                    Replayer::new(event_log.get(&ctx).await?, speed, preload).await?;

                let console = get_console_with_root(
                    invocation.trace_id,
                    console_opts.console_type,
                    ctx.verbosity,
                    true,
                    speed,
                    "(replay)", // Could be better
                    console_opts.superconsole_config(),
                    None,
                )?;

                let res = EventsCtx::new(EventSubscribers::new(vec![console]))
                    .unpack_stream::<_, ReplayResult, _>(
                        &mut NoPartialResultHandler,
                        Box::pin(replayer),
                        None,
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
                buck2_error::Ok(())
            };

            with_simple_sigint_handler(work).await.unwrap_or_else(|| {
                Err(buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Signal Interrupted"
                ))
            })
        })
        .into()
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
            Result::Error(v) => v.errors,
            Result::BuildResponse(v) => v.errors,
            Result::TestResponse(v) => v.errors,
            Result::BxlResponse(v) => v.errors,
            _ => Vec::new(),
        };

        Ok(Self { errors })
    }
}
