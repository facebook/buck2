/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#[cfg(not(windows))]
pub(crate) fn enable_ansi_support() -> bool {
    true
}

#[cfg(windows)]
pub(crate) fn enable_ansi_support() -> bool {
    // This functions has side effect: it enables ANSI support in terminals on Windows.
    // https://github.com/crossterm-rs/crossterm/blob/769b18151c6c177d43c752cc964d2b3d058bcf14/src/ansi_support.rs#L39
    // Note if this function returns false, we cannot buffer ANSI escape sequences.
    crossterm::ansi_support::supports_ansi()
}
