/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// A trait for such event subscribers that are watching a specific set of
/// errors and keeping the record of them for later use.
pub trait ErrorObserver {
    fn error_cause(&self) -> ErrorCause {
        ErrorCause::Unknown
    }

    /// Whether this observer thinks that the daemon needs killing to work again.
    fn daemon_in_memory_state_is_corrupted(&self) -> bool {
        false
    }

    /// Whether this observer thinks that the daemon needs to dump its materializer state to work
    /// again.
    fn daemon_materializer_state_is_corrupted(&self) -> bool {
        false
    }

    fn restarter_is_enabled(&self) -> bool {
        false
    }
}

pub enum ErrorCause {
    Unknown,
    Infra,
    User,
    DaemonIsBusy,
}
