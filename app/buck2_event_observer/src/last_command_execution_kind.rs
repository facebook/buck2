/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub enum LastCommandExecutionKind {
    Local,
    Remote,
    Cached,
    NoCommand,
}

/// Returns what the execution kind of the last command was in the given action.
/// It tells the execution kind of the commands that actually produced a result
/// or an error, but not the commands that fell back and were retried.
pub fn get_last_command_execution_kind(
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
