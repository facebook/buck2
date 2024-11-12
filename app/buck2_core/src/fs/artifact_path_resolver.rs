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

use crate::cells::cell_path::CellPathRef;
use crate::cells::CellResolver;
use crate::fs::buck_out_path::BuckOutPathResolver;
use crate::fs::buck_out_path::BuildArtifactPath;
use crate::fs::project::ProjectRoot;
use crate::fs::project_rel_path::ProjectRelativePathBuf;
use crate::package::source_path::SourcePathRef;

#[derive(Clone, Allocative)]
pub struct ArtifactFs {
    cell_resolver: CellResolver,
    buck_out_path_resolver: BuckOutPathResolver,
    project_filesystem: ProjectRoot,
}

impl ArtifactFs {
    pub fn new(
        buck_path_resolver: CellResolver,
        buck_out_path_resolver: BuckOutPathResolver,
        project_filesystem: ProjectRoot,
    ) -> Self {
        Self {
            cell_resolver: buck_path_resolver,
            buck_out_path_resolver,
            project_filesystem,
        }
    }

    pub fn retrieve_unhashed_location(
        &self,
        path: &BuildArtifactPath,
    ) -> Option<ProjectRelativePathBuf> {
        self.buck_out_path_resolver.unhashed_gen(path)
    }

    pub fn resolve_build(&self, path: &BuildArtifactPath) -> ProjectRelativePathBuf {
        self.buck_out_path_resolver.resolve_gen(path)
    }

    pub fn resolve_cell_path(
        &self,
        path: CellPathRef,
    ) -> buck2_error::Result<ProjectRelativePathBuf> {
        self.cell_resolver.resolve_path(path)
    }

    pub fn resolve_source(
        &self,
        source_artifact_path: SourcePathRef,
    ) -> buck2_error::Result<ProjectRelativePathBuf> {
        let cell_resolver = self.cell_resolver();
        if let Some(origin) = cell_resolver
            .get(source_artifact_path.package().cell_name())?
            .external()
        {
            Ok(self.buck_out_path_resolver.resolve_external_cell_source(
                source_artifact_path.to_cell_path().path(),
                origin.dupe(),
            ))
        } else {
            Ok(cell_resolver
                .resolve_path(source_artifact_path.package().as_cell_path())?
                .join(source_artifact_path.path()))
        }
    }

    pub fn resolve_offline_output_cache_path(
        &self,
        path: &BuildArtifactPath,
    ) -> ProjectRelativePathBuf {
        self.buck_out_path_resolver.resolve_offline_cache(path)
    }

    pub fn fs(&self) -> &ProjectRoot {
        &self.project_filesystem
    }

    pub fn buck_out_path_resolver(&self) -> &BuckOutPathResolver {
        &self.buck_out_path_resolver
    }

    pub fn cell_resolver(&self) -> &CellResolver {
        &self.cell_resolver
    }
}
