/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use dashmap::DashSet;

use crate::file_ops::RawDirEntry;
use crate::file_ops::RawPathMetadata;
use crate::io::IoProvider;

#[derive(Allocative)]
pub struct TracingIoProvider {
    io: Box<dyn IoProvider>,
    trace: DashSet<ProjectRelativePathBuf>,
}

impl TracingIoProvider {
    pub fn new(io: Box<dyn IoProvider>) -> Self {
        Self {
            io,
            trace: DashSet::new(),
        }
    }

    pub fn add_all(&self, paths: impl Iterator<Item = ProjectRelativePathBuf>) {
        for path in paths {
            self.trace.insert(path);
        }
    }

    pub fn trace(&self) -> &DashSet<ProjectRelativePathBuf> {
        &self.trace
    }
}

#[async_trait::async_trait]
impl IoProvider for TracingIoProvider {
    async fn read_file_if_exists(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<Option<String>> {
        self.trace.insert(path.clone());
        self.io.read_file_if_exists(path).await
    }

    async fn read_dir(&self, path: ProjectRelativePathBuf) -> anyhow::Result<Vec<RawDirEntry>> {
        let entries = self.io.read_dir(path.clone()).await?;
        for entry in entries.iter() {
            self.trace
                .insert(path.join(ForwardRelativePath::unchecked_new(&entry.file_name)));
        }
        Ok(entries)
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<Option<RawPathMetadata<ProjectRelativePathBuf>>> {
        self.trace.insert(path.clone());
        self.io.read_path_metadata_if_exists(path).await
    }

    async fn settle(&self) -> anyhow::Result<()> {
        self.io.settle().await
    }

    fn name(&self) -> &'static str {
        self.io.name()
    }

    async fn eden_version(&self) -> anyhow::Result<Option<String>> {
        self.io.eden_version().await
    }

    fn project_root(&self) -> &ProjectRoot {
        self.io.project_root()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
