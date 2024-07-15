/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::io::BufRead;

use allocative::Allocative;
use anyhow::Context;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::RelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use dice::DiceComputations;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::FutureExt;

use crate::dice::file_ops::DiceFileComputations;
use crate::file_ops::FileType;
use crate::file_ops::RawPathMetadata;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Allocative, derive_more::Display)]
pub enum ConfigPath {
    #[display(fmt = "{}", .0)]
    Project(ProjectRelativePathBuf),
    #[display(fmt = "{}", .0)]
    Global(AbsNormPathBuf),
}

impl ConfigPath {
    pub(crate) fn resolve_absolute(&self, project_fs: &ProjectRoot) -> AbsNormPathBuf {
        match self {
            ConfigPath::Project(path) => project_fs.resolve(path),
            ConfigPath::Global(path) => path.clone(),
        }
    }

    pub(crate) fn join_to_parent_normalized(&self, rel: &RelativePath) -> anyhow::Result<Self> {
        match self {
            ConfigPath::Project(path) => path
                .parent()
                .context("file has no parent")?
                .join_normalized(rel)
                .map(ConfigPath::Project),
            ConfigPath::Global(path) => path
                .parent()
                .context("file has no parent")?
                .join_normalized(rel)
                .map(ConfigPath::Global),
        }
    }

    pub(crate) fn join(&self, p: impl AsRef<ForwardRelativePath>) -> Self {
        match self {
            ConfigPath::Project(path) => ConfigPath::Project(path.join(p)),
            ConfigPath::Global(path) => ConfigPath::Global(path.join(p)),
        }
    }
}

pub struct ConfigDirEntry {
    pub(crate) name: FileNameBuf,
    pub(crate) is_dir: bool,
}

#[async_trait::async_trait]
#[allow(private_interfaces)]
pub trait ConfigParserFileOps: Send + Sync {
    async fn read_file_lines(
        &mut self,
        path: &ConfigPath,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Result<String, std::io::Error>> + Send>>;

    async fn file_exists(&mut self, path: &ConfigPath) -> bool;

    async fn read_dir(&mut self, path: &ConfigPath) -> anyhow::Result<Vec<ConfigDirEntry>>;
}

#[derive(buck2_error::Error, Debug)]
enum ReadDirError {
    #[error("Non-utf8 entry `{0}` in directory `{1}`")]
    NotUtf8(String, String),
}

pub(crate) struct DefaultConfigParserFileOps {
    pub(crate) project_fs: ProjectRoot,
}

#[async_trait::async_trait]
impl ConfigParserFileOps for DefaultConfigParserFileOps {
    async fn read_file_lines(
        &mut self,
        path: &ConfigPath,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Result<String, std::io::Error>> + Send>> {
        let path = path.resolve_absolute(&self.project_fs);
        let f = std::fs::File::open(&path).with_context(|| format!("Reading file `{:?}`", path))?;
        let file = std::io::BufReader::new(f);
        Ok(Box::new(file.lines()))
    }

    async fn file_exists(&mut self, path: &ConfigPath) -> bool {
        path.resolve_absolute(&self.project_fs).exists()
    }

    async fn read_dir(&mut self, path: &ConfigPath) -> anyhow::Result<Vec<ConfigDirEntry>> {
        let path = path.resolve_absolute(&self.project_fs);
        let read_dir = match std::fs::read_dir(path.as_path()) {
            Ok(read_dir) => read_dir,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
            Err(e) if e.kind() == std::io::ErrorKind::NotADirectory => {
                tracing::warn!("Expected a directory of buckconfig files at: `{}`", path);
                return Ok(Vec::new());
            }
            Err(e) => return Err(e.into()),
        };
        let mut entries = Vec::new();
        for entry in read_dir {
            let entry = entry?;
            let name = entry.file_name().into_string().map_err(|s| {
                ReadDirError::NotUtf8(
                    std::path::Path::display(s.as_ref()).to_string(),
                    path.to_string(),
                )
            })?;
            let name = FileNameBuf::try_from(name)?;
            let file_type = entry.file_type()?;
            if file_type.is_file() {
                entries.push(ConfigDirEntry {
                    name,
                    is_dir: false,
                });
            } else if file_type.is_dir() {
                entries.push(ConfigDirEntry { name, is_dir: true });
            } else {
                tracing::warn!(
                    "Expected a directory of buckconfig files at `{}`, but this entry was not a file or directory: `{}`",
                    path,
                    name,
                );
            }
        }
        Ok(entries)
    }
}

pub(crate) struct DiceConfigFileOps<'a, 'b> {
    ctx: &'a mut DiceComputations<'b>,
    cell_resolver: &'a CellResolver,
    io_ops: DefaultConfigParserFileOps,
}

impl<'a, 'b> DiceConfigFileOps<'a, 'b> {
    pub(crate) fn new(
        ctx: &'a mut DiceComputations<'b>,
        project_fs: &'a ProjectRoot,
        cell_resolver: &'a CellResolver,
    ) -> Self {
        let io_ops = DefaultConfigParserFileOps {
            project_fs: project_fs.dupe(),
        };
        Self {
            ctx,
            cell_resolver,
            io_ops,
        }
    }
}

#[async_trait::async_trait]
impl ConfigParserFileOps for DiceConfigFileOps<'_, '_> {
    async fn file_exists(&mut self, path: &ConfigPath) -> bool {
        let ConfigPath::Project(path) = path else {
            // File is outside of project root, for example, /etc/buckconfigs.d/experiments
            return self.io_ops.file_exists(path).await;
        };
        let Ok(path) = self.cell_resolver.get_cell_path(path) else {
            // Can't actually happen
            return false;
        };

        DiceFileComputations::read_path_metadata_if_exists(self.ctx, path.as_ref())
            .await
            .is_ok_and(|meta| meta.is_some())
    }

    async fn read_file_lines(
        &mut self,
        path: &ConfigPath,
    ) -> anyhow::Result<Box<(dyn Iterator<Item = Result<String, io::Error>> + Send + 'static)>>
    {
        let ConfigPath::Project(path) = path else {
            return self.io_ops.read_file_lines(path).await;
        };
        let path = self.cell_resolver.get_cell_path(path)?;
        let data = DiceFileComputations::read_file(self.ctx, path.as_ref()).await?;
        let lines = data.lines().map(ToOwned::to_owned).collect::<Vec<_>>();
        Ok(Box::new(lines.into_iter().map(Ok)))
    }

    async fn read_dir(&mut self, path: &ConfigPath) -> anyhow::Result<Vec<ConfigDirEntry>> {
        let ConfigPath::Project(path) = path else {
            return self.io_ops.read_dir(path).await;
        };
        let path = self.cell_resolver.get_cell_path(path)?;

        // This trait expects some slightly non-standard behavior wrt errors, so make sure
        // to match what the `DefaultConfigParserFileOps` do
        match DiceFileComputations::read_path_metadata_if_exists(self.ctx, path.as_ref()).await? {
            Some(RawPathMetadata::Directory) => {}
            Some(_) | None => return Ok(Vec::new()),
        }

        let out = DiceFileComputations::read_dir_include_ignores(self.ctx, path.as_ref())
            .await?
            .included
            .iter()
            .filter_map(|e| match e.file_type {
                FileType::Directory => Some(ConfigDirEntry {
                    name: e.file_name.clone(),
                    is_dir: true,
                }),
                FileType::File => Some(ConfigDirEntry {
                    name: e.file_name.clone(),
                    is_dir: false,
                }),
                FileType::Symlink | FileType::Unknown => None,
            })
            .collect();
        Ok(out)
    }
}

pub(crate) fn push_all_files_from_a_directory<'a>(
    buckconfig_paths: &'a mut Vec<ConfigPath>,
    folder_path: &'a ConfigPath,
    file_ops: &'a mut dyn ConfigParserFileOps,
) -> BoxFuture<'a, anyhow::Result<()>> {
    async move {
        for entry in file_ops.read_dir(folder_path).await? {
            let entry_path = folder_path.join(&entry.name);
            if entry.is_dir {
                push_all_files_from_a_directory(buckconfig_paths, &entry_path, file_ops).await?;
            } else {
                buckconfig_paths.push(entry_path);
            }
        }

        Ok(())
    }
    .boxed()
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::fs_util;
    use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
    use buck2_core::fs::paths::abs_path::AbsPath;

    use super::*;
    use crate::legacy_configs::cells::create_project_filesystem;

    #[test]
    fn dir_with_file() -> anyhow::Result<()> {
        let mut v = vec![];
        let dir = tempfile::tempdir()?;
        let root = AbsPath::new(dir.path())?;
        let file = root.join("foo");
        fs_util::write(&file, "")?;

        let file = AbsNormPath::new(&file)?;
        let dir = AbsNormPath::new(&dir)?;

        futures::executor::block_on(push_all_files_from_a_directory(
            &mut v,
            &ConfigPath::Global(dir.to_owned()),
            &mut DefaultConfigParserFileOps {
                project_fs: create_project_filesystem(),
            },
        ))?;
        assert_eq!(v, vec![ConfigPath::Global(file.to_owned())]);

        Ok(())
    }

    #[test]
    fn empty_dir() -> anyhow::Result<()> {
        let mut v = vec![];
        let dir = tempfile::tempdir()?;
        let dir = AbsNormPath::new(&dir)?;

        futures::executor::block_on(push_all_files_from_a_directory(
            &mut v,
            &ConfigPath::Global(dir.to_owned()),
            &mut DefaultConfigParserFileOps {
                project_fs: create_project_filesystem(),
            },
        ))?;
        assert_eq!(v, vec![]);

        Ok(())
    }

    #[test]
    fn non_existent_dir() -> anyhow::Result<()> {
        let mut v = vec![];
        let dir = tempfile::tempdir()?;
        let dir = dir.path().join("bad");
        let dir = AbsNormPath::new(&dir)?;

        futures::executor::block_on(push_all_files_from_a_directory(
            &mut v,
            &ConfigPath::Global(dir.to_owned()),
            &mut DefaultConfigParserFileOps {
                project_fs: create_project_filesystem(),
            },
        ))?;
        assert_eq!(v, vec![]);

        Ok(())
    }

    #[test]
    fn dir_in_dir() -> anyhow::Result<()> {
        let mut v = vec![];
        let dir = tempfile::tempdir()?;
        let dir = AbsPath::new(dir.path())?;
        fs_util::create_dir_all(dir.join("bad"))?;

        futures::executor::block_on(push_all_files_from_a_directory(
            &mut v,
            &ConfigPath::Global(AbsNormPath::new(dir)?.to_owned()),
            &mut DefaultConfigParserFileOps {
                project_fs: create_project_filesystem(),
            },
        ))?;
        assert_eq!(v, vec![]);

        Ok(())
    }

    #[test]
    fn file() -> anyhow::Result<()> {
        let mut v = vec![];
        let file = tempfile::NamedTempFile::new()?;
        let file = AbsNormPath::new(file.path())?;

        futures::executor::block_on(push_all_files_from_a_directory(
            &mut v,
            &ConfigPath::Global(file.to_owned()),
            &mut DefaultConfigParserFileOps {
                project_fs: create_project_filesystem(),
            },
        ))?;
        assert_eq!(v, vec![]);

        Ok(())
    }

    #[test]
    fn dir_with_file_in_dir() -> anyhow::Result<()> {
        let mut v = vec![];
        let dir = tempfile::tempdir()?;
        let dir = AbsPath::new(dir.path())?;
        let nested_dir = dir.join("nested");
        fs_util::create_dir_all(&nested_dir)?;
        let file = nested_dir.join("foo");
        fs_util::write(&file, "")?;

        let file = AbsNormPath::new(&file)?;
        let dir = AbsNormPath::new(&dir)?;

        futures::executor::block_on(push_all_files_from_a_directory(
            &mut v,
            &ConfigPath::Global(dir.to_owned()),
            &mut DefaultConfigParserFileOps {
                project_fs: create_project_filesystem(),
            },
        ))?;
        assert_eq!(v, vec![ConfigPath::Global(file.to_owned())]);

        Ok(())
    }
}
