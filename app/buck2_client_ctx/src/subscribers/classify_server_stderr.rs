/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::error::ErrorTag;

pub(crate) fn classify_server_stderr(stderr: &str) -> ErrorTag {
    if stderr.is_empty() {
        ErrorTag::ServerStderrEmpty
    } else if stderr.contains("<jemalloc>: size mismatch detected") {
        // P1181704561
        ErrorTag::ServerJemallocAssert
    } else if stderr.contains("panicked at") {
        // Sample output of `buck2 debug crash`: P1159041719
        ErrorTag::ServerPanicked
    } else if stderr.contains("has overflowed its stack") {
        // Stderr looks like this:
        // ```
        // thread 'buck2-dm' has overflowed its stack
        // ```
        ErrorTag::ServerStackOverflow
    } else if stderr.contains("Signal 11 (SIGSEGV)") {
        // P1180289404
        ErrorTag::ServerSegv
    } else {
        ErrorTag::ServerStderrUnknown
    }
}
