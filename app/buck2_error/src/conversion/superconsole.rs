/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::ErrorTag;
use crate::conversion::from_any_with_tag;

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
