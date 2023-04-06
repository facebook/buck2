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
}

pub enum ErrorCause {
    Unknown,
    Infra,
    User,
    DaemonIsBusy,
}
