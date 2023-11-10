/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::execute::request::OutputType;

/// This type intentionally does not implement `std::error::Error`. That's because it represents an
/// "incomplete" error - it needs more information like the command results, action keys, etc.
/// before it can be turned into a `buck2_build_api::actions::error::ActionError`.
#[derive(Debug)]
pub enum ExecuteError {
    MissingOutputs {
        declared: Vec<ProjectRelativePathBuf>,
    },
    MismatchedOutputs {
        declared: Vec<ProjectRelativePathBuf>,
        real: Vec<ProjectRelativePathBuf>,
    },
    WrongOutputType {
        path: ProjectRelativePathBuf,
        declared: OutputType,
        real: OutputType,
    },
    Error {
        error: anyhow::Error,
    },
    CommandExecutionError,
}

impl From<anyhow::Error> for ExecuteError {
    fn from(error: anyhow::Error) -> Self {
        let e: buck2_error::Error = error.into();
        if e.downcast_ref::<CommandExecutionErrorMarker>().is_some() {
            return Self::CommandExecutionError;
        }
        Self::Error { error: e.into() }
    }
}

#[derive(buck2_error::Error, Debug)]
#[error("Command execution failed. Details are in the command report.")]
pub struct CommandExecutionErrorMarker;
