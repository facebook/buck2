/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;

pub trait ArtifactDyn: Send + Sync + 'static {
    fn resolve_path(&self, fs: &ArtifactFs) -> anyhow::Result<ProjectRelativePathBuf>;
    fn is_source(&self) -> bool;
}
