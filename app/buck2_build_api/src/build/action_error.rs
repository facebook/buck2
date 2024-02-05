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

use crate::build::build_report::BuildReportCollector;

#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
struct BuildReportActionName {
    category: String,
    identifier: String,
}

#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
struct BuildReportActionKey {
    owner: String,
}

#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
enum BuildReportActionErrorDiagnostics {
    #[serde(rename = "sub_errors")]
    SubErrors(Vec<BuildReportActionSubError>),
    #[serde(rename = "handler_invocation_error")]
    HandlerInvocationError(String),
}

#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
struct BuildReportActionSubError {
    category: String,
    message_content: Option<String>,
    locations: Option<Vec<BuildReportActionErrorLocation>>,
}

#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
struct BuildReportActionErrorLocation {
    file: String,
    line: Option<u64>,
}

/// DO NOT UPDATE WITHOUT UPDATING `docs/users/build_observability/build_report.md`!
#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
pub(crate) struct BuildReportActionError {
    name: BuildReportActionName,
    key: BuildReportActionKey,
    digest: String,
    error_content: String,
    stderr_content: String,
    stdout_content: String,
    error_diagnostics: Option<BuildReportActionErrorDiagnostics>,
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

        let error_diagnostics = error.error_diagnostics.clone().map(|error_diagnostics| {
            match error_diagnostics.data.unwrap() {
                buck2_data::action_error_diagnostics::Data::SubErrors(sub_errors) => {
                    let sub_errors = sub_errors
                        .sub_errors
                        .iter()
                        .map(|s| {
                            let locations = s.locations.as_ref().map(|locations| {
                                locations
                                    .locations
                                    .iter()
                                    .map(|l| BuildReportActionErrorLocation {
                                        file: l.file.clone(),
                                        line: l.line,
                                    })
                                    .collect()
                            });
                            BuildReportActionSubError {
                                category: s.category.clone(),
                                message_content: s
                                    .message
                                    .clone()
                                    .map(|m| collector.update_string_cache(m)),
                                locations,
                            }
                        })
                        .collect();
                    BuildReportActionErrorDiagnostics::SubErrors(sub_errors)
                }
                buck2_data::action_error_diagnostics::Data::HandlerInvocationError(
                    invocation_failure,
                ) => BuildReportActionErrorDiagnostics::HandlerInvocationError(
                    collector.update_string_cache(invocation_failure.clone()),
                ),
            }
        });

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
            error_diagnostics,
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
