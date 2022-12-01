/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::ControlFlow;
use std::ops::FromResidual;
use std::ops::Try;

use crate::exit_result::ExitResult;

/// The final outcome returned to the client of running a command in the daemon.
///
/// Either "successful", in which case `R`, the response type, is available, or "failure",
/// where a general `CommandError` was returned. Consider this a "failed successfully" indicator.
/// At the point where this is returned, all event processing / logging should be handled.
#[derive(Debug)]
#[must_use]
pub enum CommandOutcome<R> {
    /// The buckd client successfully returned the expected response.
    ///
    /// Additional processing of this response may be necessary to determine overall success or
    /// failure within the client.
    Success(R),
    /// The buckd client successfully returned a response, but that response was a general failure.
    ///
    /// The user has already been presented an error message, and the CLI should exit with
    /// this status code.
    Failure(Option<u8>),
}

/// Small wrapper used in FromResidual
pub struct CommandFailure(Option<u8>);

/// Allow the usage of '?' when going from a CommandOutcome -> ExitResult
impl<R> Try for CommandOutcome<R> {
    type Output = R;
    type Residual = CommandFailure;

    fn from_output(output: Self::Output) -> Self {
        Self::Success(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            CommandOutcome::Success(res) => ControlFlow::Continue(res),
            CommandOutcome::Failure(status) => ControlFlow::Break(CommandFailure(status)),
        }
    }
}

impl FromResidual<CommandFailure> for ExitResult {
    fn from_residual(residual: CommandFailure) -> Self {
        ExitResult::status(residual.0.unwrap_or(1))
    }
}

impl<R> FromResidual<CommandFailure> for CommandOutcome<R> {
    fn from_residual(residual: CommandFailure) -> Self {
        Self::Failure(residual.0)
    }
}
