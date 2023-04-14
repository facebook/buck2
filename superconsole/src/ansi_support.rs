/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(not(windows))]
pub(crate) fn enable_ansi_support() -> anyhow::Result<()> {
    Ok(())
}

#[cfg(windows)]
pub(crate) fn enable_ansi_support() -> anyhow::Result<()> {
    #[derive(Debug, thiserror::Error)]
    #[error("Terminal does not support ANSI escapes")]
    struct AnsiIsNotSupported;

    // This functions has side effect: it enables ANSI support in terminals on Windows.
    // https://github.com/crossterm-rs/crossterm/blob/769b18151c6c177d43c752cc964d2b3d058bcf14/src/ansi_support.rs#L39
    // Note if this function returns false, we cannot buffer ANSI escape sequences.
    if !crossterm::ansi_support::supports_ansi() {
        return Err(AnsiIsNotSupported.into());
    }

    Ok(())
}
