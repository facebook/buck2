/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// Formats an error to be displayed at the end of the command invocation.
///
/// This function is aware of errors that may have been emitted earlier in the command execution.
pub fn late_format_error(e: &buck2_error::Error) -> Option<String> {
    if e.is_emitted() {
        // FIXME(JakobDegen): This should use consistent formatting with below
        Some(format!("{:#}", e.get_late_format()?))
    } else {
        Some(format!("{:?}", e))
    }
}
