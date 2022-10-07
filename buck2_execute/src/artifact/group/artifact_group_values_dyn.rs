/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::artifact::artifact_dyn::ArtifactDyn;
use crate::artifact::fs::ArtifactFs;
use crate::artifact_value::ArtifactValue;
use crate::directory::ActionDirectoryBuilder;

/// This is like `ArtifactGroupValues`, but without dependency on `Artifact`.
pub trait ArtifactGroupValuesDyn: Send + Sync + 'static {
    fn iter(&self) -> Box<dyn Iterator<Item = (&dyn ArtifactDyn, &ArtifactValue)> + '_>;

    fn add_to_directory(
        &self,
        builder: &mut ActionDirectoryBuilder,
        artifact_fs: &ArtifactFs,
    ) -> anyhow::Result<()>;
}
