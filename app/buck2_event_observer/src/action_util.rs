/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub fn get_action_digest(commands: &[buck2_data::CommandExecution]) -> Option<String> {
    if let Some(command_execution) = commands.last() {
        if let Some(details) = &command_execution.details {
            if let Some(command_kind) = &details.command_kind {
                if let Some(command) = &command_kind.command {
                    return match command {
                        buck2_data::command_execution_kind::Command::RemoteCommand(
                            remote_command,
                        ) => Some(remote_command.action_digest.to_owned()),
                        buck2_data::command_execution_kind::Command::LocalCommand(
                            local_command,
                        ) => Some(local_command.action_digest.to_owned()),
                        buck2_data::command_execution_kind::Command::WorkerCommand(
                            worker_command,
                        ) => Some(worker_command.action_digest.to_owned()),
                        buck2_data::command_execution_kind::Command::OmittedLocalCommand(
                            omitted_local_command,
                        ) => Some(omitted_local_command.action_digest.to_owned()),
                        _ => None,
                    };
                }
            }
        }
    }
    None
}
