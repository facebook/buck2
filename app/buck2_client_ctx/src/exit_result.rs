/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::Infallible;
use std::fmt::Display;
use std::io;
use std::io::Write;
use std::ops::FromResidual;
use std::process::Command;

use buck2_core::fs::paths::abs_path::AbsPathBuf;

pub struct ExecArgs {
    prog: String,
    argv: Vec<String>,
    chdir: Option<AbsPathBuf>,
    env: Vec<(String, String)>,
}

/// ExitResult represents the outcome of a process execution where we care to return a specific
/// exit code. This is designed to be used as the return value from `main()`.
///
/// The exit code is u8 integer and has the following meanings
/// - Success             : 0
/// - Uncategorized Error : 1
/// - Infra Error         : 2
/// - User Error          : 3
/// - Signal Interruption : 129-192 (128 + signal number)
///
/// We can easily turn a anyhow::Result (or anyhow::Error, or even a message) into a ExitResult,
/// but the reverse is not possible: once created, the only useful thing we can with a
/// ExitResult is propagate it.
#[must_use]
pub struct ExitResult {
    variant: ExitResultVariant,

    /// Some stdout output that should be emitted prior to exiting. This allows commands to buffer
    /// their final output and choose not to send it if we opt to restart the command.
    stdout: Vec<u8>,
}

enum ExitResultVariant {
    /// We finished successfully, return the specific exit code.
    Status(FailureExitCode),
    /// The command failed and it doesn't have a specific exit code yet. This may be updated by
    /// `ErrorObserver::error_cause` if more accurate categorization is available after the
    /// command ends. If no categorization succeeded, it will return exit code 1.
    UncategorizedError,
    /// Instead of terminating normally, `exec` (or spawn on Windows)
    /// a new process with the given name and argv.
    /// This is used to implement `buck2 run`.
    Buck2RunExec(ExecArgs),
    /// We failed (i.e. due to a Buck internal error).
    /// At this time, when execution does fail, we print out the error message to stderr.
    StatusWithErr(FailureExitCode, anyhow::Error),
}

impl ExitResult {
    pub fn success() -> Self {
        Self::status(FailureExitCode::Success)
    }

    pub fn status(status: FailureExitCode) -> Self {
        Self {
            variant: ExitResultVariant::Status(status),
            stdout: Vec::new(),
        }
    }

    /// Values out of the range of u8 will have their status information ignored
    pub fn status_extended(status: i32) -> Self {
        if let Ok(code) = status.try_into() {
            Self::status(FailureExitCode::Explicit(code))
        } else {
            // The exit code isn't an allowable value, so just switch to generic failure
            Self {
                variant: ExitResultVariant::UncategorizedError,
                stdout: Vec::new(),
            }
        }
    }

    pub fn exec(
        prog: String,
        argv: Vec<String>,
        chdir: Option<AbsPathBuf>,
        env: Vec<(String, String)>,
    ) -> Self {
        Self {
            variant: ExitResultVariant::Buck2RunExec(ExecArgs {
                prog,
                argv,
                chdir,
                env,
            }),
            stdout: Vec::new(),
        }
    }

    pub fn bail(msg: impl Display) -> Self {
        Self::err(anyhow::anyhow!("Command failed: {}", msg))
    }

    pub fn err(err: anyhow::Error) -> Self {
        let exit_code = if let Some(io_error) = err.downcast_ref::<UserIoError>() && io_error.0.kind() == io::ErrorKind::BrokenPipe {
            FailureExitCode::BrokenPipe
        } else {
            FailureExitCode::UnknownFailure
        };
        Self {
            variant: ExitResultVariant::StatusWithErr(exit_code, err),
            stdout: Vec::new(),
        }
    }

    pub fn err_with_exit_code(err: anyhow::Error, exit_code: FailureExitCode) -> Self {
        Self {
            variant: ExitResultVariant::StatusWithErr(exit_code, err),
            stdout: Vec::new(),
        }
    }

    /// Return this ExitStatus or call a function to produce a new one.
    pub fn or_else(self, f: impl FnOnce(Self) -> Self) -> Self {
        if matches!(
            self.variant,
            ExitResultVariant::Status(FailureExitCode::Success)
        ) {
            return self;
        }

        f(self)
    }

    pub fn with_stdout(mut self, stdout: Vec<u8>) -> Self {
        self.stdout.extend(stdout);
        self
    }

    pub fn report(self) -> ! {
        match crate::stdio::print_bytes(&self.stdout) {
            Ok(()) => self.variant.report(),
            Err(e) => Self::err(e).variant.report(),
        }
    }

    pub fn from_errors(errors: &[buck2_data::ErrorReport]) -> Self {
        let mut has_infra = false;
        let mut has_user = false;
        for e in errors {
            if e.typ == Some(buck2_data::ErrorType::DaemonIsBusy as i32) {
                return Self::status(FailureExitCode::DaemonIsBusy);
            }
            match e.category.and_then(buck2_data::ErrorCategory::from_i32) {
                Some(buck2_data::ErrorCategory::Infra) => has_infra = true,
                Some(buck2_data::ErrorCategory::User) => has_user = true,
                None => (),
            }
        }
        if has_infra {
            return Self::status(FailureExitCode::InfraError);
        }
        if has_user {
            return Self::status(FailureExitCode::UserError);
        }
        // FIXME(JakobDegen): For compatibility with pre-existing behavior, we return infra failure
        // here. However, it would be more honest to return the `1` status code that we use for
        // "unknown"
        Self::status(FailureExitCode::InfraError)
    }
}

/// We can produce a ExitResult from a `anyhow::Result` for convenience.
impl From<anyhow::Result<()>> for ExitResult {
    fn from(e: anyhow::Result<()>) -> Self {
        match e {
            Ok(()) => Self::success(),
            Err(e) => Self::err(e),
        }
    }
}

impl From<anyhow::Result<FailureExitCode>> for ExitResult {
    fn from(e: anyhow::Result<FailureExitCode>) -> Self {
        match e {
            Ok(code) => Self::status(code),
            Err(e) => Self::err(e),
        }
    }
}

impl FromResidual<anyhow::Error> for ExitResult {
    #[track_caller]
    fn from_residual(residual: anyhow::Error) -> ExitResult {
        Self::err(residual)
    }
}

impl<E: Into<::anyhow::Error>> FromResidual<Result<Infallible, E>> for ExitResult {
    #[track_caller]
    fn from_residual(residual: Result<Infallible, E>) -> ExitResult {
        match residual {
            Ok(infallible) => match infallible {},
            // E -> anyhow::Error -> ExitResult
            Err(e) => Self::err(e.into()),
        }
    }
}

/// Implementing Termination lets us set the exit code for the process.
impl ExitResultVariant {
    pub fn report(self) -> ! {
        // NOTE: We use writeln instead of println so we don't panic if stderr is closed. This
        // ensures we get the desired exit code printed instead of potentially a panic.
        let mut exit_code = match self {
            Self::Status(v) => v,
            Self::UncategorizedError => FailureExitCode::UnknownFailure,
            Self::Buck2RunExec(args) => {
                // Terminate by exec-ing a new process - usually because of `buck2 run`.
                //
                // execv does not return.
                execv(args)
            }
            Self::StatusWithErr(exit_code, e) => {
                let _ignored = writeln!(io::stderr().lock(), "Command failed: {:?}", e);
                exit_code
            }
        };

        // Global destructors in C++ dependencies destroy global state,
        // while running background threads rely on this state.
        // So the result is non-reproducible crash of the buck2 client.
        // https://fburl.com/7u7kizm7
        // So let's disable global destructors.
        // Global destructors are hard (if even possible) to do safely anyway.

        if io::stdout().flush().is_err() {
            exit_code = FailureExitCode::SignalInterrupt;
        }

        // Stderr should be autoflushed, but just in case...
        if io::stderr().flush().is_err() {
            exit_code = FailureExitCode::SignalInterrupt;
        }

        unsafe { libc::_exit(exit_code.exit_code() as libc::c_int) }
    }
}

/// A wrapper around an `io::Error` which indicates that the error came from "user IO".
///
/// We use this to inform the exit code generation
#[derive(thiserror::Error, derivative::Derivative)]
#[derivative(Debug = "transparent")]
#[error(transparent)]
pub struct UserIoError(pub io::Error);

#[derive(thiserror::Error, Debug)]
#[error("Ctrl-c was pressed")]
pub struct InterruptSignalError;

/// Common exit codes for buck with stronger semantic meanings
pub enum FailureExitCode {
    // TODO: Fill in more exit codes from ExitCode.java here. Need to determine
    // how many make sense in v2 versus v1. Some are assuredly unnecessary in v2.
    Success,
    UnknownFailure,
    InfraError,
    UserError,
    DaemonIsBusy,
    ConnectError,
    SignalInterrupt,
    BrokenPipe,
    /// Something other than buck2 itself (usually a test runner) explicitly requested that this
    /// exit code be returned
    Explicit(u8),
}

impl FailureExitCode {
    pub fn exit_code(self) -> u8 {
        use FailureExitCode::*;
        match self {
            Success => 0,
            UnknownFailure => 1,
            InfraError => 2,
            UserError => 3,
            DaemonIsBusy => 4,
            ConnectError => 11,
            BrokenPipe => 130,
            SignalInterrupt => 141,
            Explicit(code) => code,
        }
    }
}

#[cfg(windows)]
fn do_exec(command: &mut Command) -> anyhow::Error {
    let status = match command.status() {
        Ok(status) => status,
        Err(e) => return e.into(),
    };
    let code = status.code().unwrap_or(1);
    unsafe { libc::_exit(code as libc::c_int) }
}

#[cfg(unix)]
fn do_exec(command: &mut Command) -> anyhow::Error {
    use std::os::unix::process::CommandExt;

    command.exec().into()
}

/// Invokes the given program with the given argv and replaces the program image with the new program.
/// Does not return.
fn execv(args: ExecArgs) -> ! {
    let mut command = Command::new(&args.prog);
    command.args(&args.argv[1..]);
    if let Some(dir) = args.chdir {
        // Note here we break `cwd::cwd_will_not_change` promise.
        // This is OK because we don't return from this function
        // (otherwise this would be a really bad idea, even without the promise).
        command.current_dir(dir);
    }
    for (k, v) in args.env {
        // Same as above.
        command.env(k, v);
    }
    let err = do_exec(&mut command).context(format!(
        "Failed to execute target process, running {:?} {:?}",
        args.prog, args.argv
    ));
    ExitResult::err(err).report()
}
