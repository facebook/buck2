/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use superconsole::error::OutputError;

use crate::ErrorTag;
use crate::conversion::from_any_with_tag;

impl From<superconsole::error::OutputError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: superconsole::error::OutputError) -> Self {
        let i = match value {
            OutputError::Write(i) => i,
            OutputError::Terminal(i) => i,
            OutputError::SpawnThread(i) => i,
        };
        crate::Error::from(i).tag([ErrorTag::SuperConsole])
    }
}

impl<D: Into<crate::Error>> From<superconsole::Error<D>> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: superconsole::Error<D>) -> Self {
        match value {
            superconsole::Error::Output(e) => e.into(),
            superconsole::Error::Draw(e) => e.into(),
        }
    }
}

impl From<superconsole::SpanError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: superconsole::SpanError) -> Self {
        let tag = match value {
            superconsole::SpanError::InvalidWhitespace(_) => {
                ErrorTag::SuperConsoleInvalidWhitespace
            }
        };
        from_any_with_tag(value, tag)
    }
}
