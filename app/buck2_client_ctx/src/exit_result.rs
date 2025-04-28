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
use std::fmt::Display;
use std::io;
use std::io::Write;
use std::ops::FromResidual;
use std::process::Command;

use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_data::ErrorReport;
use buck2_error::ErrorTag;
use buck2_error::Tier;
use buck2_error::classify::ErrorLike;
use buck2_error::classify::best_error;
use buck2_error::conversion::from_any_with_tag;
use buck2_wrapper_common::invocation_id::TraceId;

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
/// We can easily turn a buck2_error::Result (or buck2_error::Error, or even a message) into a ExitResult,
/// but the reverse is not possible: once created, the only useful thing we can with a
/// ExitResult is propagate it.
#[must_use]
#[derive(Debug)]
pub struct ExitResult {
    variant: ExitResultVariant,

    /// Some stdout output that should be emitted prior to exiting. This allows commands to buffer
    /// their final output and choose not to send it if we opt to restart the command.
    stdout: Vec<u8>,

    // Errors that should have been written to the console already.
    // These should be written to the command report, but won't be written to the console before exiting via `report()`
    emitted_errors: Vec<ErrorReport>,
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
    StatusWithErr(ExitCode, buck2_error::Error),
}

impl ExitResult {
    pub fn success() -> Self {
        Self::status(ExitCode::Success)
    }

    pub fn is_success(&self) -> bool {
        if let ExitResultVariant::Status(ExitCode::Success) = &self.variant {
            true
        } else {
            false
        }
    }

    /// Return the stored error that hasn't been shown to the user yet, if there is one.
    pub fn get_error(&self) -> Option<buck2_error::Error> {
        if let ExitResultVariant::StatusWithErr(_, err) = &self.variant {
            Some(err.clone())
        } else {
            None
        }
    }

    /// Get all errors, emitted or not, for the command report and invocation record.
    pub fn get_all_errors(&self) -> Vec<ErrorReport> {
        let mut errors = self.emitted_errors.clone();
        if let ExitResultVariant::StatusWithErr(_, e) = &self.variant {
            errors.push(e.into());
        }
        errors
    }

    pub fn status(status: ExitCode) -> Self {
        Self {
            variant: ExitResultVariant::Status(status),
            stdout: Vec::new(),
            emitted_errors: Vec::new(),
        }
    }

    pub fn status_with_emitted_errors(status: ExitCode, emitted_errors: Vec<ErrorReport>) -> Self {
        Self {
            variant: ExitResultVariant::Status(status),
            stdout: Vec::new(),
            emitted_errors,
        }
    }

    /// Values out of the range of u8 will have their status information ignored
    pub fn status_extended(status: i32, emitted_errors: Vec<ErrorReport>) -> Self {
        let exit_code = if let Ok(code) = status.try_into() {
            ExitCode::Explicit(code)
        } else {
            // The exit code isn't an allowable value, so just switch to generic failure
            ExitCode::UnknownFailure
        };
        Self::status_with_emitted_errors(exit_code, emitted_errors)
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
            emitted_errors: Vec::new(),
        }
    }

    pub fn bail(msg: impl Display) -> Self {
        Self::err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::ActionCommandFailure,
            "Command failed: {}",
            msg
        ))
    }

    pub fn err(err: buck2_error::Error) -> Self {
        let exit_code = if err.has_tag(ErrorTag::IoClientBrokenPipe) {
            ExitCode::BrokenPipe
        } else {
            match err.get_tier() {
                Some(tier) => match tier {
                    Tier::Input => ExitCode::UserError,
                    Tier::Tier0 | Tier::Environment => ExitCode::InfraError,
                },
                None => ExitCode::UnknownFailure,
            }
        };

        Self {
            variant: ExitResultVariant::StatusWithErr(exit_code, err.into()),
            stdout: Vec::new(),
            emitted_errors: Vec::new(),
        }
    }

    pub fn err_with_exit_code(err: buck2_error::Error, exit_code: ExitCode) -> Self {
        Self {
            variant: ExitResultVariant::StatusWithErr(exit_code, err.into()),
            stdout: Vec::new(),
            emitted_errors: Vec::new(),
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

    pub fn from_command_result_errors(errors: Vec<ErrorReport>) -> Self {
        fn status_with_error_report(status: ExitCode, errors: Vec<ErrorReport>) -> ExitResult {
            ExitResult {
                variant: ExitResultVariant::Status(status),
                stdout: Vec::new(),
                emitted_errors: errors,
            }
        }

        for e in &errors {
            if e.tags.contains(&(ErrorTag::DaemonIsBusy as i32)) {
                return status_with_error_report(ExitCode::DaemonIsBusy, errors);
            }
            if e.tags.contains(&(ErrorTag::DaemonPreempted as i32)) {
                return status_with_error_report(ExitCode::DaemonPreempted, errors);
            }
        }

        match best_error(&errors).map(|error| error.category()) {
            Some(category) => match category {
                Tier::Input => status_with_error_report(ExitCode::UserError, errors),
                Tier::Tier0 | Tier::Environment => {
                    status_with_error_report(ExitCode::InfraError, errors)
                }
            },
            None => {
                // FIXME(JakobDegen): For compatibility with pre-existing behavior, we return infra failure
                // here. However, it would be more honest to return the `1` status code that we use for
                // "unknown"
                status_with_error_report(ExitCode::InfraError, errors)
            }
        }
    }

    /// Buck2 supports being built as both a "full" binary as well as a "client-only" binary.
    ///
    /// However, some commands (eg `--no-buckd`) are not supported in the client-only binary, and so
    /// when these commands are run, we have to retry them with the full build.
    ///
    /// This function is called in those cases. It returns `Some` only for client-only builds.
    pub fn retry_command_with_full_binary() -> buck2_error::Result<Option<Self>> {
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

    pub fn write_command_report(
        &self,
        trace_id: TraceId,
        buck_log_dir: Option<AbsNormPathBuf>,
        command_report_path: Option<AbsPathBuf>,
        finalizing_error_messages: Vec<String>,
    ) -> buck2_error::Result<()> {
        let (path, copy_path) = if let Some(buck_log_dir) = buck_log_dir {
            let dir = buck_log_dir.join(ForwardRelativePath::new(&trace_id.to_string())?);
            fs_util::create_dir_all(&dir)?;
            // this path is used by the buck wrapper, don't change without updating the wrapper.
            let path = dir.join(ForwardRelativePath::new("command_report.json")?);
            (path.into_abs_path_buf(), command_report_path)
        } else if let Some(command_report_path) = command_report_path {
            // If buck_log_dir is not set, we are running outside a repo, write to command_report_path if present.
            (command_report_path, None)
        } else {
            // No buck_log_dir, no command_report_path, do nothing.
            return Ok(());
        };
        let file = fs_util::create_file(&path)?;
        let mut file = std::io::BufWriter::new(file);

        let error_messages = self
            .get_all_errors()
            .iter()
            .map(|e| e.message.clone())
            .collect();

        match &self.variant {
            ExitResultVariant::Status(exit_code)
            | ExitResultVariant::StatusWithErr(exit_code, _) => {
                serde_json::to_writer_pretty(
                    &mut file,
                    &buck2_data::CommandReport {
                        trace_id: trace_id.to_string(),
                        exit_code: exit_code.exit_code(),
                        error_messages,
                        finalizing_error_messages,
                    },
                )?;

                if let Some(report_path) = copy_path {
                    if let Some(parent) = report_path.parent() {
                        fs_util::create_dir_all(parent)?;
                    }
                    // buck wrapper depends on command report being written.
                    file.flush()?;
                    fs_util::copy(path, report_path)?;
                }
            }
            _ => {}
        }

        Ok(())
    }
}

/// We can produce a ExitResult from a `buck2_error::Result` for convenience.
impl From<buck2_error::Result<()>> for ExitResult {
    fn from(e: buck2_error::Result<()>) -> Self {
        match e {
            Ok(()) => Self::success(),
            Err(e) => Self::err(e),
        }
    }
}

impl From<buck2_error::Result<ExitCode>> for ExitResult {
    fn from(e: buck2_error::Result<ExitCode>) -> Self {
        match e {
            Ok(code) => Self::status(code),
            Err(e) => Self::err(e),
        }
    }
}

impl From<buck2_error::Error> for ExitResult {
    fn from(e: buck2_error::Error) -> Self {
        Self::err(e)
    }
}

impl FromResidual<buck2_error::Error> for ExitResult {
    #[track_caller]
    fn from_residual(residual: buck2_error::Error) -> ExitResult {
        Self::err(residual)
    }
}

impl<E: Into<::buck2_error::Error>> FromResidual<Result<Infallible, E>> for ExitResult {
    #[track_caller]
    fn from_residual(residual: Result<Infallible, E>) -> ExitResult {
        match residual {
            // E -> buck2_error::Error -> ExitResult
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
pub enum ClientIoError {
    /// A broken pipe when writing to stdout is expected if stdout is closed before the command finishes.
    /// An easy way to trigger this is `buck2 audit config | head`
    #[buck2(tag = IoClientBrokenPipe)]
    #[buck2(environment)]
    BrokenPipe(io::Error),
    #[buck2(tier0)]
    OtherIo(io::Error),
    #[buck2(tier0)]
    Other(buck2_error::Error),
}

impl From<buck2_error::Error> for ClientIoError {
    fn from(error: buck2_error::Error) -> Self {
        ClientIoError::Other(error)
    }
}

impl From<csv::Error> for ClientIoError {
    fn from(error: csv::Error) -> Self {
        match error.kind() {
            csv::ErrorKind::Io(_) => {}
            _ => return ClientIoError::Other(from_any_with_tag(error, ErrorTag::CsvParse)),
        }
        match error.into_kind() {
            csv::ErrorKind::Io(io_error) => ClientIoError::from(io_error),
            // Can't clone the error or move kind and create a new csv::Error.
            _ => unreachable!("csv::Error must be an io::Error"),
        }
    }
}

impl From<serde_json::Error> for ClientIoError {
    fn from(error: serde_json::Error) -> Self {
        let error: io::Error = error.into();
        ClientIoError::from(error)
    }
}

impl From<io::Error> for ClientIoError {
    fn from(error: io::Error) -> Self {
        if error.kind() == io::ErrorKind::BrokenPipe {
            ClientIoError::BrokenPipe(error)
        } else {
            ClientIoError::OtherIo(error)
        }
    }
}

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
    pub fn exit_code(self) -> u32 {
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
            Explicit(code) => code.into(),
        }
    }
}

#[cfg(windows)]
fn do_exec(command: &mut Command) -> buck2_error::Error {
    let status = match command.status() {
        Ok(status) => status,
        Err(e) => return e.into(),
    };
    let code = status.code().unwrap_or(1);
    unsafe { libc::_exit(code as libc::c_int) }
}

#[cfg(unix)]
fn do_exec(command: &mut Command) -> buck2_error::Error {
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
