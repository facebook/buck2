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

use buck2_event_observer::event_observer::NoopEventObserverExtra;
use buck2_event_observer::verbosity::Verbosity;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;

use crate::client_ctx::ClientCommandContext;
use crate::common::ui::ConsoleType;
use crate::common::CommonEventLogOptions;
use crate::streaming::StreamingCommand;
use crate::subscribers::build_graph_stats::BuildGraphStats;
use crate::subscribers::build_id_writer::BuildIdWriter;
use crate::subscribers::errorconsole::ErrorConsole;
use crate::subscribers::event_log::EventLog;
use crate::subscribers::re_log::ReLog;
use crate::subscribers::simpleconsole::SimpleConsole;
use crate::subscribers::subscriber::EventSubscriber;
use crate::subscribers::subscriber_unpack::UnpackingEventSubscriberAsEventSubscriber;
use crate::subscribers::superconsole::StatefulSuperConsole;
use crate::subscribers::superconsole::SuperConsoleConfig;

/// Given a command name and the command arguments, create a default console / superconsole.
pub fn get_console_with_root(
    trace_id: TraceId,
    console_type: ConsoleType,
    verbosity: Verbosity,
    expect_spans: bool,
    replay_speed: Option<f64>,
    command_name: &str,
    config: SuperConsoleConfig,
) -> anyhow::Result<Box<dyn EventSubscriber>> {
    match console_type {
        ConsoleType::Simple => Ok(Box::new(UnpackingEventSubscriberAsEventSubscriber(
            SimpleConsole::<NoopEventObserverExtra>::autodetect(trace_id, verbosity, expect_spans),
        ))),
        ConsoleType::SimpleNoTty => Ok(Box::new(UnpackingEventSubscriberAsEventSubscriber(
            SimpleConsole::<NoopEventObserverExtra>::without_tty(trace_id, verbosity, expect_spans),
        ))),
        ConsoleType::SimpleTty => Ok(Box::new(UnpackingEventSubscriberAsEventSubscriber(
            SimpleConsole::<NoopEventObserverExtra>::with_tty(trace_id, verbosity, expect_spans),
        ))),
        ConsoleType::Super => Ok(Box::new(UnpackingEventSubscriberAsEventSubscriber(
            StatefulSuperConsole::new_with_root_forced(
                trace_id,
                command_name,
                verbosity,
                expect_spans,
                replay_speed,
                None,
                config,
            )?,
        ))),
        ConsoleType::Auto => {
            match StatefulSuperConsole::new_with_root(
                trace_id.dupe(),
                command_name,
                verbosity,
                expect_spans,
                replay_speed,
                config,
            )? {
                Some(super_console) => Ok(Box::new(UnpackingEventSubscriberAsEventSubscriber(
                    super_console,
                ))),
                None => Ok(Box::new(UnpackingEventSubscriberAsEventSubscriber(
                    SimpleConsole::<NoopEventObserverExtra>::autodetect(
                        trace_id,
                        verbosity,
                        expect_spans,
                    ),
                ))),
            }
        }
        ConsoleType::None => Ok(Box::new(UnpackingEventSubscriberAsEventSubscriber(
            ErrorConsole,
        ))),
    }
}

/// Given the command arguments, conditionally create an event log.
pub(crate) fn try_get_event_log_subscriber<'a, T: StreamingCommand>(
    cmd: &T,
    ctx: &ClientCommandContext<'a>,
    log_size_counter_bytes: Option<Arc<AtomicU64>>,
) -> anyhow::Result<Option<Box<dyn EventSubscriber + 'a>>> {
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
        ctx.async_cleanup_context().dupe(),
        T::COMMAND_NAME.to_owned(),
        log_size_counter_bytes,
        ctx.allow_vpnless()?,
    )?;
    Ok(Some(Box::new(log)))
}

pub(crate) fn try_get_re_log_subscriber<'a>(
    ctx: &ClientCommandContext<'a>,
) -> anyhow::Result<Option<Box<dyn EventSubscriber + 'a>>> {
    let log = ReLog::new(
        ctx.paths()?.isolation.clone(),
        ctx.async_cleanup_context().dupe(),
        ctx.allow_vpnless()?,
    );
    Ok(Some(Box::new(log)))
}

pub(crate) fn try_get_build_id_writer<'a>(
    opts: &CommonEventLogOptions,
    ctx: &ClientCommandContext<'a>,
) -> anyhow::Result<Option<Box<dyn EventSubscriber + 'a>>> {
    if let Some(file_loc) = opts.write_build_id.as_ref() {
        Ok(Some(Box::new(BuildIdWriter::new(
            file_loc.resolve(&ctx.working_dir),
        ))))
    } else {
        Ok(None)
    }
}

pub(crate) fn try_get_build_graph_stats<'a, T: StreamingCommand>(
    cmd: &T,
    ctx: &ClientCommandContext<'a>,
) -> anyhow::Result<Option<Box<dyn EventSubscriber + 'a>>> {
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
