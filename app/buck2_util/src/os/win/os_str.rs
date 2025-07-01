/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#[cfg(windows)]
pub fn os_str_to_wide_null_term(s: &std::ffi::OsStr) -> Vec<u16> {
    use std::iter;
    use std::os::windows::ffi::OsStrExt;
    s.encode_wide().chain(iter::once(0)).collect()
}
