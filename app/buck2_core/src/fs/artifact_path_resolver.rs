/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;

use crate::buck_path::path::BuckPathRef;
use crate::buck_path::resolver::BuckPathResolver;
use crate::cells::cell_path::CellPathRef;
use crate::fs::buck_out_path::BuckOutPath;
use crate::fs::buck_out_path::BuckOutPathResolver;
use crate::fs::project::ProjectRoot;
use crate::fs::project_rel_path::ProjectRelativePathBuf;

#[derive(Clone, Allocative)]
pub struct ArtifactFs {
    buck_path_resolver: BuckPathResolver,
    buck_out_path_resolver: BuckOutPathResolver,
    project_filesystem: ProjectRoot,
}

impl ArtifactFs {
    pub fn new(
        buck_path_resolver: BuckPathResolver,
        buck_out_path_resolver: BuckOutPathResolver,
        project_filesystem: ProjectRoot,
    ) -> Self {
        Self {
            buck_path_resolver,
            buck_out_path_resolver,
            project_filesystem,
        }
    }

    pub fn retrieve_unhashed_location(&self, path: &BuckOutPath) -> Option<ProjectRelativePathBuf> {
        self.buck_out_path_resolver.unhashed_gen(path)
    }

    pub fn resolve_build(&self, path: &BuckOutPath) -> ProjectRelativePathBuf {
        self.buck_out_path_resolver.resolve_gen(path)
    }

    pub fn resolve_cell_path(&self, path: CellPathRef) -> anyhow::Result<ProjectRelativePathBuf> {
        self.buck_path_resolver.resolve_cell_path(path)
    }

    pub fn resolve_source(
        &self,
        source_artifact_path: BuckPathRef,
    ) -> anyhow::Result<ProjectRelativePathBuf> {
        self.buck_path_resolver.resolve(source_artifact_path)
    }

    pub fn resolve_offline_output_cache_path(&self, path: &BuckOutPath) -> ProjectRelativePathBuf {
        self.buck_out_path_resolver.resolve_offline_cache(path)
    }

    pub fn fs(&self) -> &ProjectRoot {
        &self.project_filesystem
    }

    pub fn buck_out_path_resolver(&self) -> &BuckOutPathResolver {
        &self.buck_out_path_resolver
    }

    pub fn buck_path_resolver(&self) -> &BuckPathResolver {
        &self.buck_path_resolver
    }
}
