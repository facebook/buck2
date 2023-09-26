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
    LocalWorker,
    Remote,
    Cached,
    RemoteDepFileCached,
    NoCommand,
}

/// Returns what the execution kind of the last command was in the given action.
/// It tells the execution kind of the commands that actually produced a result
/// or an error, but not the commands that fell back and were retried.
pub fn get_last_command_execution_kind(
    action: &buck2_data::ActionExecutionEnd,
) -> LastCommandExecutionKind {
    let last_command_kind = action
        .commands
        .last()
        .and_then(|c| c.details.as_ref())
        .and_then(|c| c.command_kind.as_ref());

    let last_command = action
        .commands
        .last()
        .and_then(|c| c.details.as_ref())
        .and_then(|c| c.command.as_ref());

    if let Some(command_kind) = last_command_kind {
        use buck2_data::command_execution_kind::Command;
        match command_kind.command.as_ref() {
            Some(Command::LocalCommand(..)) | Some(Command::OmittedLocalCommand(..)) => {
                LastCommandExecutionKind::Local
            }
            Some(Command::WorkerCommand(_)) | Some(Command::WorkerInitCommand(_)) => {
                LastCommandExecutionKind::LocalWorker
            }
            Some(Command::RemoteCommand(buck2_data::RemoteCommand {
                cache_hit: true,
                cache_hit_type,
                ..
            })) => {
                match buck2_data::CacheHitType::from_i32(*cache_hit_type).unwrap() {
                    // ActionCache is 0, so this should be backwards compatible
                    buck2_data::CacheHitType::ActionCache => LastCommandExecutionKind::Cached,
                    buck2_data::CacheHitType::RemoteDepFileCache => {
                        LastCommandExecutionKind::RemoteDepFileCached
                    }
                    buck2_data::CacheHitType::Executed => LastCommandExecutionKind::Remote,
                }
            }
            Some(Command::RemoteCommand(buck2_data::RemoteCommand {
                cache_hit: false, ..
            })) => LastCommandExecutionKind::Remote,
            None => LastCommandExecutionKind::NoCommand,
        }
    } else {
        // TODO: Old format. Remove this block once the new format gets well propagated.
        use buck2_data::command_execution_details::Command;
        match last_command {
            Some(Command::LocalCommand(..)) | Some(Command::OmittedLocalCommand(..)) => {
                LastCommandExecutionKind::Local
            }
            Some(Command::WorkerCommand(_)) | Some(Command::WorkerInitCommand(_)) => {
                LastCommandExecutionKind::LocalWorker
            }
            Some(Command::RemoteCommand(buck2_data::RemoteCommand {
                cache_hit: true,
                cache_hit_type,
                ..
            })) => {
                match buck2_data::CacheHitType::from_i32(*cache_hit_type).unwrap() {
                    // ActionCache is 0, so this should be backwards compatible
                    buck2_data::CacheHitType::ActionCache => LastCommandExecutionKind::Cached,
                    buck2_data::CacheHitType::RemoteDepFileCache => {
                        LastCommandExecutionKind::RemoteDepFileCached
                    }
                    buck2_data::CacheHitType::Executed => LastCommandExecutionKind::Remote,
                }
            }
            Some(Command::RemoteCommand(buck2_data::RemoteCommand {
                cache_hit: false, ..
            })) => LastCommandExecutionKind::Remote,
            None => LastCommandExecutionKind::NoCommand,
        }
    }
}
