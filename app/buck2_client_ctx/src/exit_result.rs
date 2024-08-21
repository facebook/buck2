/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::Infallible;
use std::ffi::OsString;
use std::fmt;
use std::fmt::Display;
use std::io;
use std::io::Write;
use std::ops::FromResidual;
use std::process::Command;

use buck2_core::fs::paths::abs_path::AbsPathBuf;

#[derive(Debug)]
pub struct ExecArgs {
    prog: OsString,
    argv: Vec<OsString>,
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
#[derive(Debug)]
pub struct ExitResult {
    variant: ExitResultVariant,

    /// Some stdout output that should be emitted prior to exiting. This allows commands to buffer
    /// their final output and choose not to send it if we opt to restart the command.
    stdout: Vec<u8>,
}

#[derive(Debug)]
enum ExitResultVariant {
    /// We finished successfully, return the specific exit code.
    Status(ExitCode),
    /// Instead of terminating normally, `exec` (or spawn on Windows)
    /// a new process with the given name and argv.
    /// This is used to implement `buck2 run`.
    Exec(ExecArgs),
    /// We failed (i.e. due to a Buck internal error).
    /// At this time, when execution does fail, we print out the error message to stderr.
    StatusWithErr(ExitCode, anyhow::Error),
}

impl Display for ExitResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _ignored = match &self.variant {
            ExitResultVariant::Status(code) => write!(f, "ExitCode = {}", code.exit_code()),
            ExitResultVariant::Exec(args) => {
                write!(
                    f,
                    "Exec {} {}",
                    args.prog.to_string_lossy(),
                    args.argv
                        .iter()
                        .map(|s| s.to_string_lossy())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            ExitResultVariant::StatusWithErr(code, e) => {
                write!(f, "ExitCode = {}, Err = {}", code.exit_code(), e)
            }
        };
        if !self.stdout.is_empty() {
            let _ignored = writeln!(f, "Stdout:");
            let _ignored = write!(f, "{}", String::from_utf8_lossy(self.stdout.as_slice()));
        };
        Ok(())
    }
}

impl ExitResult {
    pub fn success() -> Self {
        Self::status(ExitCode::Success)
    }

    pub fn status(status: ExitCode) -> Self {
        Self {
            variant: ExitResultVariant::Status(status),
            stdout: Vec::new(),
        }
    }

    /// Values out of the range of u8 will have their status information ignored
    pub fn status_extended(status: i32) -> Self {
        if let Ok(code) = status.try_into() {
            Self::status(ExitCode::Explicit(code))
        } else {
            // The exit code isn't an allowable value, so just switch to generic failure
            Self::status(ExitCode::UnknownFailure)
        }
    }

    pub fn exec(
        prog: OsString,
        argv: Vec<OsString>,
        chdir: Option<AbsPathBuf>,
        env: Vec<(String, String)>,
    ) -> Self {
        Self {
            variant: ExitResultVariant::Exec(ExecArgs {
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
        let exit_code = if let Some(io_error) = err.downcast_ref::<ClientIoError>()
            && io_error.0.kind() == io::ErrorKind::BrokenPipe
        {
            ExitCode::BrokenPipe
        } else {
            ExitCode::UnknownFailure
        };
        Self {
            variant: ExitResultVariant::StatusWithErr(exit_code, err),
            stdout: Vec::new(),
        }
    }

    pub fn err_with_exit_code(err: anyhow::Error, exit_code: ExitCode) -> Self {
        Self {
            variant: ExitResultVariant::StatusWithErr(exit_code, err),
            stdout: Vec::new(),
        }
    }

    /// Return this ExitStatus or call a function to produce a new one.
    pub fn or_else(self, f: impl FnOnce(Self) -> Self) -> Self {
        if matches!(self.variant, ExitResultVariant::Status(ExitCode::Success)) {
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

    pub fn from_errors<'a>(errors: impl IntoIterator<Item = &'a buck2_data::ErrorReport>) -> Self {
        let mut has_infra = false;
        let mut has_user = false;
        for e in errors {
            if e.tags
                .contains(&(buck2_data::error::ErrorTag::DaemonIsBusy as i32))
            {
                return Self::status(ExitCode::DaemonIsBusy);
            }
            if e.tags
                .contains(&(buck2_data::error::ErrorTag::DaemonPreempted as i32))
            {
                return Self::status(ExitCode::DaemonPreempted);
            }
            match e.tier.and_then(buck2_data::error::ErrorTier::from_i32) {
                Some(buck2_data::error::ErrorTier::Tier0) => has_infra = true,
                Some(buck2_data::error::ErrorTier::Environment) => has_infra = true,
                Some(buck2_data::error::ErrorTier::Input) => has_user = true,
                Some(buck2_data::error::ErrorTier::UnusedDefaultCategory) | None => (),
            }
        }
        if has_infra {
            return Self::status(ExitCode::InfraError);
        }
        if has_user {
            return Self::status(ExitCode::UserError);
        }
        // FIXME(JakobDegen): For compatibility with pre-existing behavior, we return infra failure
        // here. However, it would be more honest to return the `1` status code that we use for
        // "unknown"
        Self::status(ExitCode::InfraError)
    }

    /// Buck2 supports being built as both a "full" binary as well as a "client-only" binary.
    ///
    /// However, some commands (eg `--no-buckd`) are not supported in the client-only binary, and so
    /// when these commands are run, we have to retry them with the full build.
    ///
    /// This function is called in those cases. It returns `Some` only for client-only builds.
    pub fn retry_command_with_full_binary() -> anyhow::Result<Option<Self>> {
        if buck2_core::client_only::is_client_only()? {
            let exe = crate::daemon::client::connect::get_daemon_exe()?;
            Ok(Some(ExitResult::exec(
                exe.into_os_string(),
                std::env::args_os().collect(),
                None,
                Vec::new(),
            )))
        } else {
            Ok(None)
        }
    }
}

impl std::error::Error for ExitResult {}

/// We can produce a ExitResult from a `anyhow::Result` for convenience.
impl From<anyhow::Result<()>> for ExitResult {
    fn from(e: anyhow::Result<()>) -> Self {
        match e {
            Ok(()) => Self::success(),
            Err(e) => Self::err(e),
        }
    }
}

impl From<anyhow::Result<ExitCode>> for ExitResult {
    fn from(e: anyhow::Result<ExitCode>) -> Self {
        match e {
            Ok(code) => Self::status(code),
            Err(e) => Self::err(e),
        }
    }
}

impl From<anyhow::Error> for ExitResult {
    fn from(e: anyhow::Error) -> Self {
        Self::err(e)
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
        // Log the exit timestamp
        tracing::debug!("Client exiting");
        // NOTE: We use writeln instead of println so we don't panic if stderr is closed. This
        // ensures we get the desired exit code printed instead of potentially a panic.
        let mut exit_code = match self {
            Self::Status(v) => v,
            Self::Exec(args) => {
                // Terminate by exec-ing a new process - usually because of `buck2 run`.
                //
                // execv does not return.
                execv(args)
            }
            Self::StatusWithErr(exit_code, e) => {
                tracing::debug!("Exiting with {:?} ({:?})", exit_code, e);

                match exit_code {
                    ExitCode::SignalInterrupt | ExitCode::BrokenPipe => {
                        // No logging for those.
                    }
                    _ => {
                        let _ignored = writeln!(io::stderr().lock(), "Command failed: {:?}", e);
                    }
                }

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
            exit_code = ExitCode::SignalInterrupt;
        }

        // Stderr should be autoflushed, but just in case...
        if io::stderr().flush().is_err() {
            exit_code = ExitCode::SignalInterrupt;
        }

        unsafe { libc::_exit(exit_code.exit_code() as libc::c_int) }
    }
}

/// A wrapper around an `io::Error` which indicates that the error came from "client IO".
///
/// We use this to inform the exit code generation
#[derive(buck2_error::Error, derivative::Derivative)]
#[derivative(Debug = "transparent")]
#[error(transparent)]
pub struct ClientIoError(pub io::Error);

/// Common exit codes for buck with stronger semantic meanings
#[derive(Clone, Copy, Debug)]
pub enum ExitCode {
    // TODO: Fill in more exit codes from ExitCode.java here. Need to determine
    // how many make sense in v2 versus v1. Some are assuredly unnecessary in v2.
    Success,
    UnknownFailure,
    InfraError,
    UserError,
    DaemonIsBusy,
    DaemonPreempted,
    Timeout,
    ConnectError,
    SignalInterrupt,
    BrokenPipe,
    /// Something other than buck2 itself (usually a test runner) explicitly requested that this
    /// exit code be returned
    Explicit(u8),
}

impl ExitCode {
    pub fn exit_code(self) -> u8 {
        use ExitCode::*;
        match self {
            Success => 0,
            UnknownFailure => 1,
            InfraError => 2,
            UserError => 3,
            DaemonIsBusy => 4,
            DaemonPreempted => 5,
            Timeout => 6,
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
    // patternlint-disable-next-line buck2-no-command-new
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
