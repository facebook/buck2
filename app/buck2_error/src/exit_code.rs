/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

/// Common exit codes for buck with stronger semantic meanings
#[derive(Clone, Copy, Debug)]
pub enum ExitCode {
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
    /// Test runner explicitly requested that this exit code be returned
    TestRunner(u8),
}

impl ExitCode {
    pub const fn exit_code(self) -> u32 {
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
            TestRunner(code) => code as u32,
        }
    }

    pub fn name(self) -> &'static str {
        use ExitCode::*;
        match self {
            Success => "SUCCESS",
            UnknownFailure => "UNKNOWN_FAILURE",
            InfraError => "INFRA_ERROR",
            UserError => "USER_ERROR",
            DaemonIsBusy => "DAEMON_IS_BUSY",
            DaemonPreempted => "DAEMON_PREEMPTED",
            Timeout => "TIMEOUT",
            ConnectError => "CONNECT_ERROR",
            BrokenPipe => "BROKEN_PIPE",
            SignalInterrupt => "SIGNAL_INTERRUPT",
            TestRunner(_) => "TEST_RUNNER",
        }
    }
}
