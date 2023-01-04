/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// Get the source control revision for this binary, if available. We provide this externally when
/// building Buck2 for release.
pub fn revision() -> Option<&'static str> {
    if let Some(rev) = std::option_env!("BUCK2_SET_EXPLICIT_VERSION") {
        if !rev.is_empty() {
            return Some(rev);
        }
    }

    None
}

/// Get the time at which this binary was built, if available.
pub fn time_iso8601() -> Option<&'static str> {
    #[cfg(any(fbcode_build, cargo_internal_build))]
    {
        Some(build_info::BuildInfo::get_time_iso8601())
    }

    #[cfg(not(any(fbcode_build, cargo_internal_build)))]
    {
        None
    }
}
