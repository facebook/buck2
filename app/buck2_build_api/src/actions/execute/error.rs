/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::Write;

use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_execute::execute::request::OutputType;
use thiserror::Error;

#[derive(Debug)]
pub enum ExecuteError {
    MissingOutputs {
        wanted: Vec<ProjectRelativePathBuf>,
    },
    MismatchedOutputs {
        wanted: Vec<ProjectRelativePathBuf>,
        got: Vec<ProjectRelativePathBuf>,
    },
    WrongOutputType {
        path: ProjectRelativePathBuf,
        wanted: OutputType,
        got: OutputType,
    },
    Error {
        error: anyhow::Error,
    },
    CommandExecutionError,
}

impl ExecuteError {
    pub(crate) fn as_proto(&self) -> buck2_data::action_execution_end::Error {
        match self {
            ExecuteError::MissingOutputs { wanted } => buck2_data::CommandOutputsMissing {
                message: format!("Action failed to produce outputs: {}", error_items(wanted)),
            }
            .into(),
            ExecuteError::MismatchedOutputs { wanted, got } => buck2_data::CommandOutputsMissing {
                message: format!(
                    "Action didn't produce the right set of outputs.\nExpected {}`\nGot {}",
                    error_items(wanted),
                    error_items(got)
                ),
            }
            .into(),
            ExecuteError::WrongOutputType {path, wanted, got} => buck2_data::CommandOutputsMissing {
                message: format!(
                    "Action didn't produce output of the right type.\nExpected {path} to be {wanted:?}\nGot {got:?}",
                ),
            }
            .into(),
            ExecuteError::Error { error } => format!("{:#}", error).into(),
            ExecuteError::CommandExecutionError => buck2_data::CommandExecutionError {}.into(),
        }
    }
}

fn error_items<T: Display>(xs: &[T]) -> String {
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

impl From<anyhow::Error> for ExecuteError {
    fn from(error: anyhow::Error) -> Self {
        if error.is::<CommandExecutionErrorMarker>() {
            return Self::CommandExecutionError;
        }
        Self::Error { error }
    }
}

#[derive(Error, Debug)]
#[error("Command execution failed. Details are in the command report.")]
pub struct CommandExecutionErrorMarker;
