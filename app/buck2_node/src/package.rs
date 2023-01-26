/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::build_file_path::BuildFilePath;

/// Package-specific data for `TargetNode`.
///
/// (Note this has nothing to do with `PACKAGE` files which are not implemented
/// at the moment of writing.)
#[derive(Debug, Hash, Allocative, Eq, PartialEq)]
pub struct Package {
    /// The build file which defined this target, e.g. `fbcode//foo/bar/TARGETS`
    pub buildfile_path: Arc<BuildFilePath>,
    /// The oncall attribute, if set
    pub oncall: Option<Arc<String>>,
    /// Visibility is public by default.
    pub default_visibility_to_public: bool,
}
