/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::sync::Arc;

use dupe::Dupe;

use crate::cells::name::CellName;

#[derive(Debug, Clone, Dupe, allocative::Allocative, PartialEq, Eq)]
pub enum ExternalCellOrigin {
    Bundled(CellName),
    Git(GitCellSetup),
}

#[derive(
    Debug,
    derive_more::Display,
    Clone,
    Dupe,
    allocative::Allocative,
    PartialEq,
    Eq,
    Hash
)]
#[display("git({}, {})", git_origin, commit)]
pub struct GitCellSetup {
    pub git_origin: Arc<str>,
    // Guaranteed to be a valid sha1 commit hash
    pub commit: Arc<str>,
}

impl fmt::Display for ExternalCellOrigin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bundled(cell) => write!(f, "bundled({})", cell),
            Self::Git(git) => write!(f, "{}", git),
        }
    }
}
