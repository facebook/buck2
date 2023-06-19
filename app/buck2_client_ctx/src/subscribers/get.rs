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

use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_event_observer::event_observer::NoopEventObserverExtra;
use buck2_event_observer::verbosity::Verbosity;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;

use crate::argv::SanitizedArgv;
use crate::client_ctx::ClientCommandContext;
use crate::common::CommonDaemonCommandOptions;
use crate::common::ConsoleType;
use crate::path_arg::PathArg;
use crate::subscribers::build_id_writer::BuildIdWriter;
use crate::subscribers::event_log::subscriber::EventLog;
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
    show_waiting_message: bool,
    replay_speed: Option<f64>,
    command_name: &str,
    config: SuperConsoleConfig,
    isolation_dir: FileNameBuf,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    match console_type {
        ConsoleType::Simple => Ok(Some(Box::new(UnpackingEventSubscriberAsEventSubscriber(
            SimpleConsole::<NoopEventObserverExtra>::autodetect(
                trace_id,
                isolation_dir,
                verbosity,
                show_waiting_message,
            ),
        )))),
        ConsoleType::SimpleNoTty => Ok(Some(Box::new(UnpackingEventSubscriberAsEventSubscriber(
            SimpleConsole::<NoopEventObserverExtra>::without_tty(
                trace_id,
                isolation_dir,
                verbosity,
                show_waiting_message,
            ),
        )))),
        ConsoleType::SimpleTty => Ok(Some(Box::new(UnpackingEventSubscriberAsEventSubscriber(
            SimpleConsole::<NoopEventObserverExtra>::with_tty(
                trace_id,
                isolation_dir,
                verbosity,
                show_waiting_message,
            ),
        )))),
        ConsoleType::Super => Ok(Some(Box::new(UnpackingEventSubscriberAsEventSubscriber(
            StatefulSuperConsole::new_with_root_forced(
                trace_id,
                command_name,
                verbosity,
                show_waiting_message,
                replay_speed,
                None,
                config,
                isolation_dir,
            )?,
        )))),
        ConsoleType::Auto => {
            match StatefulSuperConsole::new_with_root(
                trace_id.dupe(),
                command_name,
                verbosity,
                show_waiting_message,
                replay_speed,
                config,
                isolation_dir.clone(),
            )? {
                Some(super_console) => Ok(Some(Box::new(
                    UnpackingEventSubscriberAsEventSubscriber(super_console),
                ))),
                None => Ok(Some(Box::new(UnpackingEventSubscriberAsEventSubscriber(
                    SimpleConsole::<NoopEventObserverExtra>::autodetect(
                        trace_id,
                        isolation_dir,
                        verbosity,
                        show_waiting_message,
                    ),
                )))),
            }
        }
        ConsoleType::None => Ok(None),
    }
}

/// Given the command arguments, conditionally create an event log.
pub(crate) fn try_get_event_log_subscriber<'a>(
    event_log_opts: &CommonDaemonCommandOptions,
    sanitized_argv: SanitizedArgv,
    ctx: &ClientCommandContext<'a>,
    log_size_counter_bytes: Option<Arc<AtomicU64>>,
    user_event_log: &Option<PathArg>,
) -> anyhow::Result<Option<Box<dyn EventSubscriber + 'a>>> {
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
        ctx.command_name.clone(),
        log_size_counter_bytes,
    )?;
    Ok(Some(Box::new(log)))
}

pub(crate) fn try_get_re_log_subscriber<'a>(
    ctx: &ClientCommandContext<'a>,
) -> anyhow::Result<Option<Box<dyn EventSubscriber + 'a>>> {
    let log = ReLog::new(
        ctx.paths()?.isolation.clone(),
        ctx.async_cleanup_context().dupe(),
    );
    Ok(Some(Box::new(log)))
}

pub(crate) fn try_get_build_id_writer<'a>(
    opts: &CommonDaemonCommandOptions,
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
