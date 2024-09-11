/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(windows)]
pub fn os_str_to_wide_null_term(s: &std::ffi::OsStr) -> Vec<u16> {
    use std::iter;
    use std::os::windows::ffi::OsStrExt;
    s.encode_wide().chain(iter::once(0)).collect()
}
