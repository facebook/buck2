/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#[cfg(unix)]
impl From<nix::errno::Errno> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: nix::errno::Errno) -> Self {
        let error = crate::conversion::from_any_with_tag(value, crate::ErrorTag::Nix);
        error.string_tag(&format!("{:?}", value))
    }
}
