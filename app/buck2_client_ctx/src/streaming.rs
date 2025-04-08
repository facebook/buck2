/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::AtomicU64;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
use dupe::Dupe;

use crate::client_ctx::BuckSubcommand;
use crate::client_ctx::ClientCommandContext;
use crate::common::ui::get_console_with_root;
use crate::common::ui::CommonConsoleOptions;
use crate::common::BuckArgMatches;
use crate::common::CommonBuildConfigurationOptions;
use crate::common::CommonEventLogOptions;
use crate::common::CommonStarlarkOptions;
use crate::daemon::client::connect::connect_buckd;
use crate::daemon::client::connect::BuckdConnectConstraints;
use crate::daemon::client::connect::DaemonConstraintsRequest;
use crate::daemon::client::connect::DesiredTraceIoState;
use crate::daemon::client::BuckdClientConnector;
use crate::events_ctx::EventsCtx;
use crate::exit_result::ExitCode;
use crate::exit_result::ExitResult;
use crate::path_arg::PathArg;
use crate::signal_handler::with_simple_sigint_handler;
use crate::subscribers::build_graph_stats::BuildGraphStats;
use crate::subscribers::build_id_writer::BuildIdWriter;
use crate::subscribers::event_log::EventLog;
use crate::subscribers::health_check_subscriber::HealthCheckSubscriber;
use crate::subscribers::re_log::ReLog;
use crate::subscribers::recorder::try_get_invocation_recorder;
use crate::subscribers::subscriber::EventSubscriber;
use crate::subscribers::subscribers::EventSubscribers;

const HEALTH_CHECK_CHANNEL_SIZE: usize = 100;
fn default_subscribers<T: StreamingCommand>(
    cmd: &T,
    matches: BuckArgMatches<'_>,
    ctx: &ClientCommandContext,
) -> buck2_error::Result<EventSubscribers> {
    let console_opts = cmd.console_opts();
    let mut subscribers = vec![];
    let expect_spans = cmd.should_expect_spans();

    // Need this to get information from one subscriber (event_log)
    // and log it in another (invocation_recorder)
    let log_size_counter_bytes = Some(Arc::new(AtomicU64::new(0)));

    let (health_check_tags_sender, health_check_tags_receiver) =
        tokio::sync::mpsc::channel(HEALTH_CHECK_CHANNEL_SIZE);
    let (health_check_display_reports_sender, health_check_display_reports_receiver) =
        tokio::sync::mpsc::channel(HEALTH_CHECK_CHANNEL_SIZE);

    subscribers.push(get_console_with_root(
        ctx.trace_id.dupe(),
        console_opts.console_type,
        ctx.verbosity,
        expect_spans,
        None,
        T::COMMAND_NAME,
        console_opts.superconsole_config(),
        Some(health_check_display_reports_receiver),
    )?);

    if let Some(event_log) = try_get_event_log_subscriber(cmd, ctx, log_size_counter_bytes.clone())?
    {
        subscribers.push(event_log)
    }
    if let Some(re_log) = try_get_re_log_subscriber(ctx)? {
        subscribers.push(re_log)
    }
    if let Some(build_id_writer) = try_get_build_id_writer(cmd.event_log_opts(), ctx)? {
        subscribers.push(build_id_writer)
    }
    if let Some(build_graph_stats) = try_get_build_graph_stats(cmd, ctx)? {
        subscribers.push(build_graph_stats)
    }
    let representative_config_flags = if ctx.maybe_paths()?.is_some() {
        matches.get_representative_config_flags()?
    } else {
        Vec::new()
    };
    let mut recorder = try_get_invocation_recorder(
        ctx,
        cmd.event_log_opts(),
        cmd.logging_name(),
        cmd.sanitize_argv(ctx.argv.clone()).argv,
        representative_config_flags,
        log_size_counter_bytes,
        Some(health_check_tags_receiver),
    )?;
    recorder.update_metadata_from_client_metadata(&ctx.client_metadata);
    subscribers.push(recorder);

    subscribers.push(HealthCheckSubscriber::new(
        ctx.trace_id.dupe(),
        health_check_tags_sender,
        health_check_display_reports_sender,
    ));
    subscribers.extend(cmd.extra_subscribers());
    Ok(EventSubscribers::new(subscribers))
}

/// Trait to generalize the behavior of executable buck2 commands that rely on a server.
/// This trait is most helpful when the command wants a superconsole, to stream events, etc.
/// However, this is the most robustly tested of our code paths, and there is little cost to defaulting to it.
/// As a result, prefer to default to streaming mode unless there is a compelling reason not to
/// (e.g `status`)
#[async_trait]
pub trait StreamingCommand: Sized + Send + Sync {
    const COMMAND_NAME: &'static str;

    /// Run the command.
    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: BuckArgMatches<'_>,
        ctx: &mut ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult;

    /// Should we only connect to existing servers (`true`), or spawn a new server if required (`false`).
    /// Defaults to `false`.
    fn existing_only() -> bool {
        false
    }

    fn trace_io(&self) -> DesiredTraceIoState {
        DesiredTraceIoState::Existing
    }

    fn console_opts(&self) -> &CommonConsoleOptions;

    fn event_log_opts(&self) -> &CommonEventLogOptions;

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions;

    fn starlark_opts(&self) -> &CommonStarlarkOptions;

    fn extra_subscribers(&self) -> Vec<Box<dyn EventSubscriber>> {
        vec![]
    }

    fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }

    /// Some commands should always be displaying
    /// at least 1 ongoing process in the terminal, aka "spans".
    /// In the simple console, we want to display a "Waiting for daemon..." message
    /// exclusively for these commands whenever there are long periods without spans.
    fn should_expect_spans(&self) -> bool {
        true
    }

    /// Currently only for BxlCommand.
    fn user_event_log(&self) -> &Option<PathArg> {
        &None
    }
}

impl<T: StreamingCommand> BuckSubcommand for T {
    const COMMAND_NAME: &'static str = T::COMMAND_NAME;

    /// Actual call that runs a `StreamingCommand`.
    /// Handles the business of setting up a server connection for streaming.
    async fn exec_impl(
        self,
        matches: BuckArgMatches<'_>,
        mut ctx: ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let work = async {
            let mut constraints = if T::existing_only() {
                BuckdConnectConstraints::ExistingOnly
            } else {
                let mut req =
                    DaemonConstraintsRequest::new(ctx.immediate_config, T::trace_io(&self))?;
                ctx.restarter.apply_to_constraints(&mut req);
                BuckdConnectConstraints::Constraints(req)
            };
            let buckd = match ctx.start_in_process_daemon.take() {
                None => connect_buckd(constraints, events_ctx, ctx.paths()?).await,
                Some(start_in_process_daemon) => {
                    // Start in-process daemon, wait until it is ready to accept connections.
                    start_in_process_daemon()?;

                    // Do not attempt to spawn a daemon if connect failed.
                    // Connect should not fail.
                    constraints = BuckdConnectConstraints::ExistingOnly;

                    connect_buckd(constraints, events_ctx, ctx.paths()?).await
                }
            };

            let mut buckd = match buckd {
                Ok(buckd) => buckd,
                Err(e) => {
                    return ExitResult::err_with_exit_code(e, ExitCode::ConnectError);
                }
            };

            let command_result = self
                .exec_impl(&mut buckd, matches, &mut ctx, events_ctx)
                .await;

            ctx.restarter.observe(&buckd, events_ctx);

            command_result
        };

        // FIXME: move this into client_ctx
        with_simple_sigint_handler(work)
            .await
            .unwrap_or_else(|| ExitResult::status(ExitCode::SignalInterrupt))
    }

    fn subscribers(
        &self,
        matches: BuckArgMatches<'_>,
        ctx: &ClientCommandContext,
    ) -> buck2_error::Result<EventSubscribers> {
        default_subscribers(self, matches, ctx)
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        self.event_log_opts()
    }

    fn logging_name(&self) -> &'static str {
        Self::COMMAND_NAME
    }
}

/// Given the command arguments, conditionally create an event log.
fn try_get_event_log_subscriber<T: StreamingCommand>(
    cmd: &T,
    ctx: &ClientCommandContext,
    log_size_counter_bytes: Option<Arc<AtomicU64>>,
) -> buck2_error::Result<Option<Box<dyn EventSubscriber>>> {
    let event_log_opts = cmd.event_log_opts();
    let sanitized_argv = cmd.sanitize_argv(ctx.argv.clone());
    let user_event_log = cmd.user_event_log();

    if event_log_opts.no_event_log {
        return Ok(None);
    }
    let logdir = ctx.paths()?.log_dir();
    let log = EventLog::new(
        logdir,
        ctx.working_dir.clone(),
        event_log_opts
            .event_log
            .as_ref()
            .map(|p| p.resolve(&ctx.working_dir)),
        user_event_log.as_ref().map(|p| p.resolve(&ctx.working_dir)),
        sanitized_argv,
        T::COMMAND_NAME.to_owned(),
        log_size_counter_bytes,
    )?;
    Ok(Some(Box::new(log)))
}

fn try_get_re_log_subscriber(
    ctx: &ClientCommandContext,
) -> buck2_error::Result<Option<Box<dyn EventSubscriber>>> {
    let log = ReLog::new(ctx.paths()?.isolation.clone());
    Ok(Some(Box::new(log)))
}

fn try_get_build_id_writer(
    opts: &CommonEventLogOptions,
    ctx: &ClientCommandContext,
) -> buck2_error::Result<Option<Box<dyn EventSubscriber>>> {
    if let Some(file_loc) = opts.write_build_id.as_ref() {
        Ok(Some(Box::new(BuildIdWriter::new(
            file_loc.resolve(&ctx.working_dir),
        ))))
    } else {
        Ok(None)
    }
}

fn try_get_build_graph_stats<T: StreamingCommand>(
    cmd: &T,
    ctx: &ClientCommandContext,
) -> buck2_error::Result<Option<Box<dyn EventSubscriber>>> {
    if should_handle_build_graph_stats(cmd) {
        Ok(Some(Box::new(BuildGraphStats::new(
            ctx.fbinit(),
            ctx.trace_id.dupe(),
        ))))
    } else {
        Ok(None)
    }
}

fn should_handle_build_graph_stats<T: StreamingCommand>(cmd: &T) -> bool {
    // Currently, we only care about graph size info in BuildResponse which build command produces
    cmd.build_config_opts()
        .config_values
        .contains(&"buck2.log_configured_graph_size=true".to_owned())
        && cmd.logging_name() == "build"
}
