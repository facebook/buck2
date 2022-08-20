/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod build_id_writer;
pub mod display;
pub mod event_log;
mod re;
pub(crate) mod recorder;
mod simpleconsole;
pub mod span_tracker;
pub(crate) mod stdout_stderr_forwarder;
pub mod superconsole;

use ::superconsole::Component;
use buck2_events::subscriber::EventSubscriber;
pub(crate) use event_log::EventLog;
use gazebo::prelude::*;
pub(crate) use simpleconsole::SimpleConsole;

use crate::commands::common::subscribers::build_id_writer::BuildIdWriter;
use crate::commands::common::subscribers::superconsole::StatefulSuperConsole;
use crate::commands::common::verbosity::Verbosity;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::commands::common::ConsoleType;
use crate::ClientCommandContext;

/// Given a command name and the command arguments, create a default console / superconsole.
pub(crate) fn get_console_with_root(
    console_type: ConsoleType,
    verbosity: Verbosity,
    show_waiting_message: bool,
    replay_speed: Option<f64>,
    root: Box<dyn Component>,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    match console_type {
        ConsoleType::Simple => Ok(Some(box SimpleConsole::autodetect(
            verbosity,
            show_waiting_message,
        ))),
        ConsoleType::SimpleNoTty => Ok(Some(box SimpleConsole::without_tty(
            verbosity,
            show_waiting_message,
        ))),
        ConsoleType::SimpleTty => Ok(Some(box SimpleConsole::with_tty(
            verbosity,
            show_waiting_message,
        ))),
        ConsoleType::Super => Ok(Some(box StatefulSuperConsole::new_with_root_forced(
            root,
            verbosity,
            show_waiting_message,
            replay_speed,
        )?)),
        ConsoleType::Auto => {
            match StatefulSuperConsole::new_with_root(
                root,
                verbosity,
                show_waiting_message,
                replay_speed,
            )? {
                Some(super_console) => Ok(Some(box super_console)),
                None => Ok(Some(box SimpleConsole::autodetect(
                    verbosity,
                    show_waiting_message,
                ))),
            }
        }
        ConsoleType::None => Ok(None),
    }
}

/// Given the command arguments, conditionally create an event log.
pub(crate) fn try_get_event_log_subscriber(
    event_log_opts: &CommonDaemonCommandOptions,
    ctx: &ClientCommandContext,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    if event_log_opts.no_event_log {
        return Ok(None);
    }
    let logdir = ctx.paths()?.log_dir();
    let log = EventLog::new(
        logdir,
        event_log_opts.event_log.clone(),
        ctx.async_cleanup_context().dupe(),
    )?;
    Ok(Some(box log))
}

pub(crate) fn try_get_build_id_writer(
    opts: &CommonDaemonCommandOptions,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    if let Some(file_loc) = opts.build_id_file.as_ref() {
        Ok(Some(box BuildIdWriter::new(file_loc.clone())))
    } else {
        Ok(None)
    }
}

enum LastCommandExecutionKind {
    Local,
    Remote,
    Cached,
    NoCommand,
}

/// Returns what the execution kind of the last command was in the given action.
/// It tells the execution kind of the commands that actually produced a result
/// or an error, but not the commands that fell back and were retried.
fn get_last_command_execution_kind(
    action: &buck2_data::ActionExecutionEnd,
) -> LastCommandExecutionKind {
    use buck2_data::command_execution_details::Command;

    let last_command = action
        .commands
        .last()
        .and_then(|c| c.details.as_ref())
        .and_then(|c| c.command.as_ref());

    match last_command {
        Some(Command::LocalCommand(..)) | Some(Command::OmittedLocalCommand(..)) => {
            LastCommandExecutionKind::Local
        }
        Some(Command::RemoteCommand(buck2_data::RemoteCommand {
            cache_hit: true, ..
        })) => LastCommandExecutionKind::Cached,
        Some(Command::RemoteCommand(buck2_data::RemoteCommand {
            cache_hit: false, ..
        })) => LastCommandExecutionKind::Remote,
        None => LastCommandExecutionKind::NoCommand,
    }
}
