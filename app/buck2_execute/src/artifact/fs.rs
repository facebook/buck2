/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_common::executor_config::PathSeparatorKind;
use buck2_core::buck_path::path::BuckPathRef;
use buck2_core::buck_path::resolver::BuckPathResolver;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_core::fs::buck_out_path::BuckOutPathResolver;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use either::Either;

use crate::path::artifact_path::ArtifactPath;

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

    pub fn resolve(&self, artifact: ArtifactPath<'_>) -> anyhow::Result<ProjectRelativePathBuf> {
        let ArtifactPath {
            base_path,
            projected_path,
            hidden_components_count: _,
        } = artifact;

        let base_path = match base_path {
            Either::Left(build) => self.buck_out_path_resolver.resolve_gen(&build),
            Either::Right(source) => self.buck_path_resolver.resolve(source)?,
        };

        Ok(match projected_path {
            Some(projected_path) => base_path.join(projected_path),
            None => base_path,
        })
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

    pub fn fs(&self) -> &ProjectRoot {
        &self.project_filesystem
    }

    pub fn buck_out_path_resolver(&self) -> &BuckOutPathResolver {
        &self.buck_out_path_resolver
    }
}

pub struct ExecutorFs<'a> {
    fs: &'a ArtifactFs,
    path_separator: PathSeparatorKind,
}

impl<'a> ExecutorFs<'a> {
    pub fn new(fs: &'a ArtifactFs, path_separator: PathSeparatorKind) -> Self {
        Self { fs, path_separator }
    }

    pub fn fs(&self) -> &ArtifactFs {
        self.fs
    }

    pub fn path_separator(&self) -> PathSeparatorKind {
        self.path_separator
    }
}
