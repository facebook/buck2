/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Schema for the structured action error within the build report.

use buck2_data::command_execution_kind::Command;
use buck2_data::ActionError;
use buck2_data::CommandExecutionDetails;
use buck2_event_observer::display::display_action_owner;
use buck2_event_observer::display::get_action_error_reason;
use buck2_event_observer::display::TargetDisplayOptions;
use serde::Serialize;

use crate::commands::build::BuildReportCollector;

#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
struct BuildReportActionName {
    category: String,
    identifier: String,
}

#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
struct BuildReportActionKey {
    owner: String,
}

/// DO NOT UPDATE WITHOUT UPDATING `docs/users/build_observability/build_report.md`!
#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
struct BuildReportActionError {
    name: BuildReportActionName,
    key: BuildReportActionKey,
    digest: String,
    error_content: u64,
    stderr_content: u64,
    stdout_content: u64,
}

impl BuildReportActionError {
    pub(crate) fn new<'a>(error: &ActionError, collector: &mut BuildReportCollector<'a>) -> Self {
        let reason = get_action_error_reason(error).ok().unwrap_or_default();

        let command_details = error.last_command.as_ref().and_then(|c| c.details.as_ref());

        let owner = error.key.as_ref().map_or(String::default(), |key| {
            key.owner.as_ref().map_or(String::default(), |owner| {
                display_action_owner(owner, TargetDisplayOptions::for_build_report())
                    .unwrap_or_default()
            })
        });

        let key = BuildReportActionKey { owner };

        let name = BuildReportActionName {
            category: error
                .name
                .as_ref()
                .map_or(String::default(), |name| name.category.clone()),
            identifier: error
                .name
                .as_ref()
                .map_or(String::default(), |name| name.identifier.clone()),
        };

        let stderr = command_details.map_or(String::default(), |c| c.stderr.clone());
        let stdout = command_details.map_or(String::default(), |c| c.stdout.clone());

        let error_content = collector.update_string_cache(reason);
        let stderr_content = collector.update_string_cache(stderr);
        let stdout_content = collector.update_string_cache(stdout);

        Self {
            key,
            name,
            error_content,
            stderr_content,
            stdout_content,
            digest: get_action_digest(command_details).unwrap_or_default(),
        }
    }
}

fn get_action_digest(command_details: Option<&CommandExecutionDetails>) -> Option<String> {
    command_details.and_then(|command_details| {
        if let Some(command_kind) = &command_details.command_kind {
            match command_kind.command.as_ref() {
                Some(Command::LocalCommand(c)) => Some(c.action_digest.clone()),
                Some(Command::OmittedLocalCommand(c)) => Some(c.action_digest.clone()),
                Some(Command::WorkerCommand(c)) => Some(c.action_digest.clone()),
                Some(Command::WorkerInitCommand(_)) => None,
                Some(Command::RemoteCommand(c)) => Some(c.action_digest.clone()),
                None => None,
            }
        } else {
            None
        }
    })
}
