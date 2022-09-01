/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::project::ProjectRelativePathBuf;
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

    Error {
        error: anyhow::Error,
    },

    CommandExecutionError,
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
