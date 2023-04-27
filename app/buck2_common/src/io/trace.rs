/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;

use allocative::Allocative;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use dashmap::DashSet;

use crate::file_ops::RawDirEntry;
use crate::file_ops::RawPathMetadata;
use crate::file_ops::RawSymlink;
use crate::io::IoProvider;

#[derive(Allocative, Debug, Hash, PartialEq, Eq, Clone)]
pub struct Symlink {
    pub at: ProjectRelativePathBuf,
    pub to: RawSymlink<ProjectRelativePathBuf>,
}

#[derive(Allocative)]
pub struct Trace {
    pub project_entries: DashSet<ProjectRelativePathBuf>,
    pub buck_out_entries: DashSet<ProjectRelativePathBuf>,
    pub external_entries: DashSet<AbsNormPathBuf>,
    pub symlinks: DashSet<Symlink>,
}

impl Trace {
    pub fn new() -> Self {
        Self {
            project_entries: DashSet::new(),
            buck_out_entries: DashSet::new(),
            external_entries: DashSet::new(),
            symlinks: DashSet::new(),
        }
    }

    /// Provides an owned view of the underlying trace state.
    pub fn project_entries(&self) -> Vec<ProjectRelativePathBuf> {
        self.project_entries
            .iter()
            .map(|path| path.key().to_buf())
            .collect()
    }

    pub fn buck_out_entries(&self) -> Vec<ProjectRelativePathBuf> {
        self.buck_out_entries
            .iter()
            .map(|path| path.key().to_buf())
            .collect()
    }

    pub fn external_entries(&self) -> Vec<AbsNormPathBuf> {
        self.external_entries
            .iter()
            .map(|path| path.key().to_owned())
            .collect()
    }
}

#[derive(Allocative)]
pub struct TracingIoProvider {
    io: Box<dyn IoProvider>,
    trace: Trace,
}

impl TracingIoProvider {
    pub fn new(io: Box<dyn IoProvider>) -> Self {
        Self {
            io,
            trace: Trace::new(),
        }
    }

    pub fn add_project_path(&self, path: ProjectRelativePathBuf) {
        self.trace.project_entries.insert(path);
    }

    pub fn add_buck_out_entry(&self, entry: ProjectRelativePathBuf) {
        self.trace.buck_out_entries.insert(entry);
    }

    pub fn add_external_path(&self, path: AbsNormPathBuf) {
        self.trace.external_entries.insert(path);
    }

    pub fn add_symlink(&self, link: Symlink) {
        self.trace.symlinks.insert(link);
    }

    pub fn add_config_paths<I>(&self, project_root: &ProjectRoot, paths: I)
    where
        I: IntoIterator<Item = AbsNormPathBuf>,
    {
        for abspath in paths.into_iter() {
            if let Ok(project_path) = project_root.relativize(&abspath).map(Cow::into_owned) {
                self.add_project_path(project_path);
            } else {
                self.add_external_path(abspath);
            }
        }
    }

    pub fn trace(&self) -> &Trace {
        &self.trace
    }
}

#[async_trait::async_trait]
impl IoProvider for TracingIoProvider {
    /// Combination of read_file_if_exists from underlying fs struct and reading
    /// the metadata. This is done so we get accurate path classification (e.g.
    /// if the path is a real file/dir or a symlink pointing somewhere else).
    ///
    /// This makes code working with the exported I/O manifest much easier to
    /// work with at the expense of some additional I/O during tracing builds.
    async fn read_file_if_exists(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<Option<String>> {
        self.add_project_path(path.clone());
        self.io.read_file_if_exists(path).await
    }

    /// Combination of read_file_if_exists from underlying fs struct and reading
    /// the metadata. This is done so we get accurate path classification (e.g.
    /// if the path is a real file/dir or a symlink pointing somewhere else).
    ///
    /// This makes code working with the exported I/O manifest much easier to
    /// work with at the expense of some additional I/O during tracing builds.
    async fn read_dir(&self, path: ProjectRelativePathBuf) -> anyhow::Result<Vec<RawDirEntry>> {
        let entries = self.io.read_dir(path.clone()).await?;
        self.add_project_path(path.clone());
        for entry in entries.iter() {
            self.add_project_path(path.join(ForwardRelativePath::unchecked_new(&entry.file_name)));
        }

        Ok(entries)
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<Option<RawPathMetadata<ProjectRelativePathBuf>>> {
        let res = self.io.read_path_metadata_if_exists(path.clone()).await?;
        match &res {
            Some(RawPathMetadata::File(_)) | Some(RawPathMetadata::Directory) => {
                self.add_project_path(path);
            }
            Some(RawPathMetadata::Symlink { at, to }) => {
                self.add_symlink(Symlink {
                    at: at.clone(),
                    to: to.clone(),
                });
            }
            _ => {}
        }

        Ok(res)
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
