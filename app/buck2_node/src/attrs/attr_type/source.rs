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

#[derive(Debug, Eq, PartialEq, Hash, Allocative, Clone, Copy, Dupe)]
pub struct SourceAttrType {
    pub allow_directory: bool,
}
