/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use dupe::Dupe;
use starlark::eval::ProfileMode;

/// How individual starlark invocation (`bzl`, `BUCK` or analysis) should be interpreted.
#[derive(Clone, Dupe, Eq, PartialEq, Allocative)]
pub enum StarlarkProfileMode {
    None,
    Profile(ProfileMode),
}

impl StarlarkProfileMode {
    pub fn profile_mode(&self) -> Option<&ProfileMode> {
        match self {
            StarlarkProfileMode::Profile(profile) => Some(profile),
            StarlarkProfileMode::None => None,
        }
    }
}
