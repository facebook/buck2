/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::Infallible;
use std::ffi::CString;
use std::fmt::Display;
use std::io::Write;
use std::io::{self};
use std::ops::ControlFlow;
use std::ops::FromResidual;
use std::ops::Try;
use std::process::ExitCode;
use std::process::Termination;

use anyhow::Context;
use cli_proto::command_result;
use gazebo::prelude::*;

/// ExitResult represents the outcome of a process execution where we care to return a specific
/// exit code. This is designed to be used as the return value from `main()`.
///
/// The exit code is u8 integer and has the following meanings
/// - Success             : 0
/// - Uncategorized Error : 1
/// - User Error          : 2-128
/// - Signal Interruption : 129-192 (128 + signal number)
/// - Infra Error         : 200-255
///
/// We can easily turn a anyhow::Result (or anyhow::Error, or even a message) into a ExitResult,
/// but the reverse is not possible: once created, the only useful thing we can with a
/// ExitResult is propagate it.
pub enum ExitResult {
    /// We finished successfully, return the specific exit code.
    Status(u8),
    /// Instead of terminating normally, `exec` a new process with the given name and argv.
    Exec(String, Vec<String>),
    /// We failed (i.e. due to a Buck internal error).
    /// At this time, when execution does fail, we print out the error message to stderr.
    Err(anyhow::Error),
}

impl ExitResult {
    pub fn success() -> Self {
        Self::Status(0)
    }

    pub fn failure() -> Self {
        Self::Status(1)
    }

    pub fn status(status: u8) -> Self {
        Self::Status(status)
    }

    /// Values out of the range of u8 will have their status information ignored
    pub fn status_extended(status: i32) -> Self {
        if let Ok(code) = status.try_into() {
            Self::status(code)
        } else {
            // The exit code isn't an allowable value, so just switch to generic failure
            Self::failure()
        }
    }

    pub fn exec(prog: String, argv: Vec<String>) -> Self {
        Self::Exec(prog, argv)
    }

    pub fn bail(msg: impl Display) -> Self {
        Self::Err(anyhow::anyhow!("Command failed: {}", msg))
    }

    pub fn infer(result: &command_result::Result) -> Self {
        let exit_code = match result {
            command_result::Result::BuildResponse(response) => {
                if response.error_messages.is_empty() {
                    0
                } else {
                    1
                }
            }
            _ => 0,
        };
        Self::status(exit_code)
    }

    pub fn is_success(&self) -> bool {
        if let ExitResult::Status(exit_code) = self {
            return *exit_code == 0;
        }
        false
    }
}

/// We can produce a ExitResult from a `anyhow::Result` for convenience.
impl From<::anyhow::Result<()>> for ExitResult {
    fn from(e: ::anyhow::Result<()>) -> Self {
        match e {
            Ok(()) => Self::success(),
            Err(e) => Self::Err(e),
        }
    }
}

impl From<::anyhow::Result<u8>> for ExitResult {
    fn from(e: ::anyhow::Result<u8>) -> Self {
        match e {
            Ok(code) => Self::status(code),
            Err(e) => Self::Err(e),
        }
    }
}

impl From<FailureExitCode> for ExitResult {
    fn from(e: FailureExitCode) -> Self {
        Self::Err(e.into())
    }
}

/// Implementing Try allows us to use a ExitResult as the outcome of a function and still use
/// the `?` operator.
impl Try for ExitResult {
    type Output = u8;
    type Residual = anyhow::Error;

    fn from_output(output: Self::Output) -> Self {
        Self::Status(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Self::Status(v) => ControlFlow::Continue(v),
            Self::Err(v) => ControlFlow::Break(v),
            // `Exec` doesn't lend itself to a reasonable implementation of Try; it doesn't easily decompose into a
            // residual or output, and changing the output type would break all call sites of ExitResult.
            Self::Exec(_, _) => unimplemented!("Try impl invoked on Exec variant"),
        }
    }
}

impl FromResidual<anyhow::Error> for ExitResult {
    #[track_caller]
    fn from_residual(residual: anyhow::Error) -> ExitResult {
        Self::Err(residual)
    }
}

impl<E: Into<::anyhow::Error>> FromResidual<Result<Infallible, E>> for ExitResult {
    #[track_caller]
    fn from_residual(residual: Result<Infallible, E>) -> ExitResult {
        match residual {
            Ok(infallible) => match infallible {},
            // E -> anyhow::Error -> ExitResult
            Err(e) => Self::Err(e.into()),
        }
    }
}

/// Implementing Termination lets us set the exit code for the process.
impl Termination for ExitResult {
    fn report(self) -> ExitCode {
        // NOTE: We use writeln! instead of println! so we don't panic if stderr is closed. This
        // ensures we get the desired exit code printed instead of potentially a panic.
        match self {
            Self::Status(v) => ExitCode::from(v),
            Self::Exec(prog, argv) => {
                // Terminate by exec-ing a new process - usually because of `buck2 run`.
                //
                // execv does not return on successful operation, so it always returns an error.
                let e = execv(&prog, &argv).unwrap_err();
                Self::Err(e).report()
            }
            Self::Err(e) => {
                match e.downcast_ref::<FailureExitCode>() {
                    None => {
                        let _ignored = writeln!(io::stderr().lock(), "Command failed: {:?}", e);
                        ExitCode::FAILURE
                    }
                    Some(FailureExitCode::SignalInterrupt) => {
                        tracing::debug!("Interrupted");
                        ExitCode::from(130)
                    }
                    Some(FailureExitCode::StdoutBrokenPipe) => {
                        // Report a broken pipe, but don't print anything to stderr by default. If
                        // the user wants to find out why we exited non-zero, they'll have to look
                        // at the output or raise the log level.
                        tracing::debug!("stdout pipe was broken");
                        ExitCode::from(141)
                    }
                    Some(FailureExitCode::StderrBrokenPipe) => {
                        // Not much point in printing anything here, since we know stderr is
                        // closed.
                        ExitCode::from(141)
                    }
                    Some(FailureExitCode::OutputFileBrokenPipe) => {
                        tracing::debug!("--out pipe was broken");
                        ExitCode::from(141)
                    }
                }
            }
        }
    }
}

/// Common exit codes for buck with stronger semantic meanings
#[derive(thiserror::Error, Debug, Copy, Clone, Dupe)]
pub enum FailureExitCode {
    // TODO: Fill in more exit codes from ExitCode.java here. Need to determine
    // how many make sense in v2 versus v1. Some are assuredly unnecessary in v2.
    #[error("Ctrl-c was pressed")]
    SignalInterrupt,

    #[error("Broken pipe writing on stdout")]
    StdoutBrokenPipe,

    #[error("Broken pipe writing on stdout")]
    StderrBrokenPipe,

    #[error("Broken pipe writing build artifact to --out")]
    OutputFileBrokenPipe,
}

/// Invokes the given program with the given argv and replaces the program image with the new program. Does not return
/// in the case of successful execution.
fn execv(prog: &str, argv: &[String]) -> anyhow::Result<()> {
    let argv_cstrs: Vec<CString> = argv.try_map(|s| CString::new(s.clone()))?;
    let mut argv_ptrs: Vec<*const i8> = argv_cstrs.map(|cstr| cstr.as_ptr());
    // By convention, execv's second argument is terminated by a null pointer.
    argv_ptrs.push(std::ptr::null());
    let prog_cstr = CString::new(prog).context("program name contained a null byte")?;
    unsafe {
        libc::execv(prog_cstr.as_ptr(), argv_ptrs.as_ptr());
    }

    // `execv` never returns on success; on failure, it sets errno.
    Err(std::io::Error::last_os_error().into())
}
