/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use ::superconsole::Component;
use gazebo::prelude::*;

use crate::client_ctx::ClientCommandContext;
use crate::common::CommonDaemonCommandOptions;
use crate::common::ConsoleType;
use crate::subscribers::build_id_writer::BuildIdWriter;
use crate::subscribers::event_log::EventLog;
use crate::subscribers::re_log::ReLog;
use crate::subscribers::simpleconsole::SimpleConsole;
use crate::subscribers::subscriber::EventSubscriber;
use crate::subscribers::subscriber_unpack::UnpackingEventSubscriberAsEventSubscriber;
use crate::subscribers::superconsole::StatefulSuperConsole;
use crate::subscribers::superconsole::SuperConsoleConfig;
use crate::verbosity::Verbosity;

/// Given a command name and the command arguments, create a default console / superconsole.
pub(crate) fn get_console_with_root(
    console_type: ConsoleType,
    verbosity: Verbosity,
    show_waiting_message: bool,
    replay_speed: Option<f64>,
    root: Box<dyn Component>,
    config: SuperConsoleConfig,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    match console_type {
        ConsoleType::Simple => Ok(Some(box UnpackingEventSubscriberAsEventSubscriber(
            SimpleConsole::autodetect(verbosity, show_waiting_message),
        ))),
        ConsoleType::SimpleNoTty => Ok(Some(box UnpackingEventSubscriberAsEventSubscriber(
            SimpleConsole::without_tty(verbosity, show_waiting_message),
        ))),
        ConsoleType::SimpleTty => Ok(Some(box UnpackingEventSubscriberAsEventSubscriber(
            SimpleConsole::with_tty(verbosity, show_waiting_message),
        ))),
        ConsoleType::Super => Ok(Some(box UnpackingEventSubscriberAsEventSubscriber(
            StatefulSuperConsole::new_with_root_forced(
                root,
                verbosity,
                show_waiting_message,
                replay_speed,
                None,
                config,
            )?,
        ))),
        ConsoleType::Auto => {
            match StatefulSuperConsole::new_with_root(
                root,
                verbosity,
                show_waiting_message,
                replay_speed,
                config,
            )? {
                Some(super_console) => Ok(Some(box UnpackingEventSubscriberAsEventSubscriber(
                    super_console,
                ))),
                None => Ok(Some(box UnpackingEventSubscriberAsEventSubscriber(
                    SimpleConsole::autodetect(verbosity, show_waiting_message),
                ))),
            }
        }
        ConsoleType::None => Ok(None),
    }
}

/// Given the command arguments, conditionally create an event log.
pub(crate) fn try_get_event_log_subscriber(
    event_log_opts: &CommonDaemonCommandOptions,
    sanitized_argv: Vec<String>,
    ctx: &ClientCommandContext,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    if event_log_opts.no_event_log {
        return Ok(None);
    }
    if ctx.replayer.is_some() {
        // We don't want to write logs when replaying command
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
        sanitized_argv,
        ctx.async_cleanup_context().dupe(),
        ctx.command_name.clone(),
    )?;
    Ok(Some(box log))
}

pub(crate) fn try_get_re_log_subscriber(
    ctx: &ClientCommandContext,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    if ctx.replayer.is_some() {
        // We don't want to upload logs when replaying command
        return Ok(None);
    }
    let log = ReLog::new(
        ctx.paths()?.isolation.clone(),
        ctx.async_cleanup_context().dupe(),
    );
    Ok(Some(box UnpackingEventSubscriberAsEventSubscriber(log)))
}

pub(crate) fn try_get_build_id_writer(
    opts: &CommonDaemonCommandOptions,
    ctx: &ClientCommandContext,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    if let Some(file_loc) = opts.build_id_file.as_ref() {
        Ok(Some(box BuildIdWriter::new(
            file_loc.resolve(&ctx.working_dir),
        )))
    } else {
        Ok(None)
    }
}
