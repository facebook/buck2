/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod display;
pub mod event_log;
mod simpleconsole;
pub mod span_tracker;
pub(crate) mod stdout_stderr_forwarder;
pub mod superconsole;

use ::superconsole::Component;
pub use event_log::EventLog;
use events::subscriber::EventSubscriber;
use gazebo::prelude::*;
pub use simpleconsole::SimpleConsole;

pub use crate::commands::common::subscribers::superconsole::SuperConsole;
use crate::{
    commands::common::{
        subscribers::superconsole::StatefulSuperConsole, verbosity::Verbosity,
        CommonEventLogOptions, ConsoleType,
    },
    CommandContext,
};

/// Given a command name and the command arguments, create a default console / superconsole.
pub fn get_console_with_root(
    console_type: ConsoleType,
    verbosity: Verbosity,
    replay_speed: Option<f64>,
    root: Box<dyn Component>,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    match console_type {
        ConsoleType::Simple => Ok(Some(box SimpleConsole::autodetect(verbosity))),
        ConsoleType::SimpleNoTty => Ok(Some(box SimpleConsole::without_tty(verbosity))),
        ConsoleType::SimpleTty => Ok(Some(box SimpleConsole::with_tty(verbosity))),
        ConsoleType::Super => Ok(Some(box StatefulSuperConsole::new_with_root_forced(
            root,
            verbosity,
            replay_speed,
        )?)),
        ConsoleType::Auto => {
            match StatefulSuperConsole::new_with_root(root, verbosity, replay_speed)? {
                Some(super_console) => Ok(Some(box super_console)),
                None => Ok(Some(box SimpleConsole::autodetect(verbosity))),
            }
        }
        ConsoleType::None => Ok(None),
    }
}

/// Given the command arguments, conditionally create an event log.
pub fn try_get_event_log_subscriber(
    event_log_opts: &CommonEventLogOptions,
    ctx: &CommandContext,
) -> anyhow::Result<Option<Box<dyn EventSubscriber>>> {
    if event_log_opts.no_event_log {
        return Ok(None);
    }
    let logdir = ctx.paths()?.log_dir();
    Ok(Some(box EventLog::new(
        logdir,
        event_log_opts.event_log.clone(),
        ctx.async_cleanup_context().dupe(),
    )))
}
