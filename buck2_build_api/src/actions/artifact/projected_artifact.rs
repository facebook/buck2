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
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use derive_more::Display;
use gazebo::prelude::*;

use crate::actions::artifact::BaseArtifactKind;

/// A path within another Artifact.
#[derive(Clone, Debug, Display, Dupe, Hash, PartialEq, Eq, Allocative)]
#[display(fmt = "{}/{}", base, path)]
pub struct ProjectedArtifact {
    base: BaseArtifactKind,
    path: Arc<ForwardRelativePathBuf>,
}

impl ProjectedArtifact {
    pub fn new(base: BaseArtifactKind, path: Arc<ForwardRelativePathBuf>) -> Self {
        Self { base, path }
    }

    pub fn base(&self) -> &BaseArtifactKind {
        &self.base
    }

    pub fn path(&self) -> &ForwardRelativePath {
        &self.path
    }
}
