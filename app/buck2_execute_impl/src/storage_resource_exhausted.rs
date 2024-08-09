/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_execute::re::error::RemoteExecutionError;
use remote_execution::TCode;
use remote_execution::TStatus;

#[cfg_attr(not(fbcode_build), allow(dead_code))]
pub(crate) trait REErrorWithCodeAndMessage {
    fn message(&self) -> &str;
    fn code(&self) -> &TCode;
}

impl REErrorWithCodeAndMessage for RemoteExecutionError {
    fn message(&self) -> &str {
        &self.message
    }

    fn code(&self) -> &TCode {
        &self.code
    }
}

impl REErrorWithCodeAndMessage for TStatus {
    fn message(&self) -> &str {
        &self.message
    }

    fn code(&self) -> &TCode {
        &self.code
    }
}

pub(crate) fn is_storage_resource_exhausted<T: REErrorWithCodeAndMessage>(err: &T) -> bool {
    #[cfg(fbcode_build)]
    {
        use once_cell::sync::Lazy;
        use regex::Regex;

        fn regex() -> &'static Regex {
            // Taken from https://fburl.com/code/7n3qg2jj
            static RE: Lazy<Regex> =
                Lazy::new(|| Regex::new(r"^.*has exceeded quota.*DemandControl.*$").unwrap());
            &RE
        }

        if *err.code() != TCode::RESOURCE_EXHAUSTED {
            return false;
        }
        let message = err.message();
        if message.contains("CAS resource exhausted") {
            return true;
        }
        if message.contains("Use case throttling") {
            return true;
        }
        if regex().is_match(message) {
            return true;
        }
        false
    }

    #[cfg(not(fbcode_build))]
    {
        let _ignored = err;
        false
    }
}
