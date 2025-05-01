/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use dice::DiceComputations;
use dupe::Dupe;

use crate::context::HasBuildContextData;

#[async_trait]
pub trait GetArtifactFs {
    /// Get the configured ArtifactFs.
    async fn get_artifact_fs(&mut self) -> buck2_error::Result<ArtifactFs>;
}

#[async_trait]
impl GetArtifactFs for DiceComputations<'_> {
    async fn get_artifact_fs(&mut self) -> buck2_error::Result<ArtifactFs> {
        let buck_out_path_resolver = self.get_buck_out_path().await?;
        let project_filesystem = self.global_data().get_io_provider().project_root().dupe();
        let buck_path_resolver = self.get_cell_resolver().await?;
        Ok(ArtifactFs::new(
            buck_path_resolver,
            buck_out_path_resolver,
            project_filesystem,
        ))
    }
}
