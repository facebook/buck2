/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::cells::paths::CellRelativePath;

/// This is `CellRelativePath` which may contain incorrect path elements.
#[repr(transparent)]
pub struct UncheckedCellRelativePath(str);

impl UncheckedCellRelativePath {
    #[inline]
    pub fn new(path: &CellRelativePath) -> &UncheckedCellRelativePath {
        Self::unchecked_new(path.as_str())
    }

    #[inline]
    pub fn unchecked_new(path: &str) -> &UncheckedCellRelativePath {
        unsafe {
            // SAFETY: `repr(transparent)`.
            &*(path as *const str as *const UncheckedCellRelativePath)
        }
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}
