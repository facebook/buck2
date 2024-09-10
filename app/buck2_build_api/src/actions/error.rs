/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use buck2_event_observer::display::display_action_error;
use buck2_event_observer::display::TargetDisplayOptions;

use crate::actions::execute::error::ExecuteError;

#[derive(Debug)]
pub struct ActionError {
    execute_error: ExecuteError,
    name: buck2_data::ActionName,
    key: buck2_data::ActionKey,
    last_command: Option<buck2_data::CommandExecution>,
    error_diagnostics: Option<buck2_data::ActionErrorDiagnostics>,
}

impl std::error::Error for ActionError {
    fn provide<'a>(&'a self, request: &mut std::error::Request<'a>) {
        if let ExecuteError::Error { error } = &self.execute_error {
            error.provide(request);
        }

        let is_command_failure = self.last_command.as_ref().is_some_and(|c| {
            matches!(
                c.status,
                Some(buck2_data::command_execution::Status::Failure { .. })
            )
        });

        let typ = match &self.execute_error {
            ExecuteError::CommandExecutionError { .. } => {
                if is_command_failure {
                    Some(buck2_error::ErrorType::ActionCommandFailure)
                } else {
                    None
                }
            }
            _ => None,
        };

        let mut tags = vec![buck2_error::ErrorTag::AnyActionExecution];

        let category = match &self.execute_error {
            ExecuteError::CommandExecutionError { error } => {
                if let Some(err) = error {
                    tags.extend(err.tags());
                }

                if is_command_failure {
                    Some(buck2_error::Tier::Input)
                } else {
                    None
                }
            }
            // Returning extra outputs is a bug in the executor
            ExecuteError::MismatchedOutputs { .. } => Some(buck2_error::Tier::Tier0),
            // However outputs may be legitimately missing if the action didn't produce them
            ExecuteError::MissingOutputs { .. } => Some(buck2_error::Tier::Input),
            // Or if the action produced the wrong type
            ExecuteError::WrongOutputType { .. } => Some(buck2_error::Tier::Input),
            ExecuteError::Error { .. } => None,
        };

        buck2_error::provide_metadata(
            request,
            category,
            typ,
            tags,
            std::file!(),
            Some("ActionError"),
            Some(self.as_proto_event()),
        );
    }

    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.execute_error {
            ExecuteError::Error { error } => error.source(),
            _ => None,
        }
    }
}

impl fmt::Display for ActionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = display_action_error(&self.as_proto_event(), TargetDisplayOptions::for_log())
            .expect("Action key is always present in `ActionError`")
            .simple_format_for_build_report();
        write!(f, "{}", s)
    }
}

impl ActionError {
    pub(crate) fn new(
        execute_error: ExecuteError,
        name: buck2_data::ActionName,
        key: buck2_data::ActionKey,
        last_command: Option<buck2_data::CommandExecution>,
        error_diagnostics: Option<buck2_data::ActionErrorDiagnostics>,
    ) -> Self {
        Self {
            execute_error,
            name,
            key,
            last_command,
            error_diagnostics,
        }
    }

    pub(crate) fn as_proto_field(&self) -> buck2_data::action_execution_end::Error {
        match &self.execute_error {
            ExecuteError::MissingOutputs { declared } => buck2_data::CommandOutputsMissing {
                message: format!("Action failed to produce outputs: {}", error_items(declared)),
            }
            .into(),
            ExecuteError::MismatchedOutputs { declared, real } => buck2_data::CommandOutputsMissing {
                message: format!(
                    "Action didn't produce the right set of outputs.\nExpected {}`\nreal {}",
                    error_items(declared),
                    error_items(real)
                ),
            }
            .into(),
            ExecuteError::WrongOutputType {path, declared, real} => buck2_data::CommandOutputsMissing {
                message: format!(
                    "Action didn't produce output of the right type.\nExpected {path} to be {declared:?}\nreal {real:?}",
                ),
            }
            .into(),
            ExecuteError::Error { error } => format!("{:#}", error).into(),
            ExecuteError::CommandExecutionError { .. } => buck2_data::CommandExecutionError {}.into(),
        }
    }

    pub(crate) fn as_proto_event(&self) -> buck2_data::ActionError {
        let field = match self.as_proto_field() {
            buck2_data::action_execution_end::Error::Unknown(e) => e.into(),
            buck2_data::action_execution_end::Error::MissingOutputs(e) => e.into(),
            buck2_data::action_execution_end::Error::CommandExecutionError(e) => e.into(),
        };
        buck2_data::ActionError {
            error: Some(field),
            name: Some(self.name.clone()),
            key: Some(self.key.clone()),
            last_command: self.last_command.clone(),
            error_diagnostics: self.error_diagnostics.clone(),
        }
    }
}

fn error_items<T: fmt::Display>(xs: &[T]) -> String {
    use fmt::Write;

    if xs.is_empty() {
        return "none".to_owned();
    }
    let mut res = String::new();
    for (i, x) in xs.iter().enumerate() {
        if i != 0 {
            res.push_str(", ");
        }
        write!(res, "`{}`", x).unwrap();
    }
    res
}
