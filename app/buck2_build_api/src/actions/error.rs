/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use buck2_error::ErrorTag;
use buck2_error::__for_macro::AsDynError;
use buck2_error::source_location::SourceLocation;
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
        let is_command_failure = self.last_command.as_ref().is_some_and(|c| {
            matches!(
                c.status,
                Some(buck2_data::command_execution::Status::Failure { .. })
            )
        });

        let mut tags = vec![ErrorTag::AnyActionExecution];
        let mut source_location = SourceLocation::new(std::file!()).with_type_name("ActionError");
        match &self.execute_error {
            ExecuteError::CommandExecutionError { error } => {
                if let Some(err) = error {
                    tags.extend(err.tags());
                    source_location = err.source_location().clone();
                }

                if is_command_failure {
                    tags.push(ErrorTag::ActionCommandFailure)
                }
            }
            // Returning extra outputs is a bug in the executor
            ExecuteError::MismatchedOutputs { .. } => tags.push(ErrorTag::ActionMismatchedOutputs),
            // However outputs may be legitimately missing if the action didn't produce them
            ExecuteError::MissingOutputs { .. } => tags.push(ErrorTag::ActionMissingOutputs),
            // Or if the action produced the wrong type
            ExecuteError::WrongOutputType { .. } => tags.push(ErrorTag::ActionWrongOutputType),
            ExecuteError::Error { error } => {
                tags.extend(error.tags());
                source_location = error.source_location().clone();
            }
        };

        buck2_error::provide_metadata(request, tags, source_location, Some(self.as_proto_event()));
    }

    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.execute_error {
            ExecuteError::Error { error } => Some(error.as_dyn_error()),
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

#[cfg(test)]
mod tests {
    use buck2_error::buck2_error;
    use buck2_error::conversion::from_any_with_tag;
    use buck2_error::ErrorTag;

    use super::*;

    #[test]
    fn test_error_conversion() {
        let error = buck2_error!(ErrorTag::Http, "error");

        let execute_error = ExecuteError::Error {
            error: error.into(),
        };

        let action_error = ActionError::new(
            execute_error,
            buck2_data::ActionName {
                category: "category".to_owned(),
                identifier: "identifier".to_owned(),
            },
            buck2_data::ActionKey {
                id: vec![],
                key: "key".to_owned(),
                owner: Some(buck2_data::action_key::Owner::TargetLabel(
                    buck2_data::ConfiguredTargetLabel {
                        label: Some(buck2_data::TargetLabel {
                            package: "package".to_owned(),
                            name: "name".to_owned(),
                        }),
                        configuration: Some(buck2_data::Configuration {
                            full_name: "conf".into(),
                        }),
                        execution_configuration: None,
                    },
                )),
            },
            None,
            None,
        );

        let buck2_error = from_any_with_tag(action_error, ErrorTag::AnyActionExecution);

        assert_eq!(
            buck2_error.tags(),
            vec![ErrorTag::AnyActionExecution, ErrorTag::Http]
        );
    }
}
