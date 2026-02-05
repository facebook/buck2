/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::io::BufRead;

use allocative::Allocative;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_fs::IoResultExt;
use buck2_fs::fs_util;
use buck2_fs::fs_util::IoError;
use buck2_fs::paths::RelativePath;
use buck2_fs::paths::abs_path::AbsPathBuf;
use buck2_fs::paths::file_name::FileNameBuf;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use dice::DiceComputations;
use dupe::Dupe;
use futures::FutureExt;
use futures::future::BoxFuture;

use crate::file_ops::dice::DiceFileComputations;
use crate::file_ops::metadata::FileType;
use crate::file_ops::metadata::RawPathMetadata;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Allocative, derive_more::Display)]
pub enum ConfigPath {
    #[display("{}", _0)]
    Project(ProjectRelativePathBuf),
    #[display("{}", _0)]
    Global(AbsPathBuf),
}

impl ConfigPath {
    pub(crate) fn resolve_absolute(&self, project_fs: &ProjectRoot) -> AbsPathBuf {
        match self {
            ConfigPath::Project(path) => project_fs.resolve(path).into_abs_path_buf(),
            ConfigPath::Global(path) => path.clone(),
        }
    }

    pub(crate) fn join_to_parent_normalized(
        &self,
        rel: &RelativePath,
    ) -> buck2_error::Result<Self> {
        match self {
            ConfigPath::Project(path) => Ok(path
                .parent()
                .buck_error_context("file has no parent")?
                .join_normalized(rel)
                .map(ConfigPath::Project)?),
            ConfigPath::Global(path) => Ok(ConfigPath::Global(
                path.parent()
                    .buck_error_context("file has no parent")?
                    .join(rel.as_str()),
            )),
        }
    }

    pub(crate) fn join(&self, p: impl AsRef<ForwardRelativePath>) -> Self {
        match self {
            ConfigPath::Project(path) => ConfigPath::Project(path.join(p)),
            ConfigPath::Global(path) => ConfigPath::Global(path.join(p.as_ref().as_path())),
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
    async fn read_file_lines_if_exists(
        &mut self,
        path: &ConfigPath,
    ) -> buck2_error::Result<Option<Vec<String>>>;

    async fn read_dir(&mut self, path: &ConfigPath) -> buck2_error::Result<Vec<ConfigDirEntry>>;
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum ReadDirError {
    #[error("Non-utf8 entry `{0}` in directory `{1}`")]
    NotUtf8(String, String),
}

pub(crate) struct DefaultConfigParserFileOps {
    pub(crate) project_fs: ProjectRoot,
}

#[async_trait::async_trait]
impl ConfigParserFileOps for DefaultConfigParserFileOps {
    async fn read_file_lines_if_exists(
        &mut self,
        path: &ConfigPath,
    ) -> buck2_error::Result<Option<Vec<String>>> {
        let path = path.resolve_absolute(&self.project_fs);
        let Some(f) = fs_util::open_file_if_exists(&path)
            .with_buck_error_context(|| format!("Reading file `{path:?}`"))?
        else {
            return Ok(None);
        };
        let file = std::io::BufReader::new(f);

        let lines = file
            .lines()
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| IoError::new_with_path("read_line", path, e))
            .categorize_internal()?;

        Ok(Some(lines))
    }

    async fn read_dir(&mut self, path: &ConfigPath) -> buck2_error::Result<Vec<ConfigDirEntry>> {
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
    async fn read_file_lines_if_exists(
        &mut self,
        path: &ConfigPath,
    ) -> buck2_error::Result<Option<Vec<String>>> {
        let ConfigPath::Project(path) = path else {
            return self.io_ops.read_file_lines_if_exists(path).await;
        };
        let path = self.cell_resolver.get_cell_path(path);
        let Some(data) = DiceFileComputations::read_file_if_exists(self.ctx, path.as_ref()).await?
        else {
            return Ok(None);
        };
        let lines = data.lines().map(ToOwned::to_owned).collect::<Vec<_>>();
        Ok(Some(lines))
    }

    async fn read_dir(&mut self, path: &ConfigPath) -> buck2_error::Result<Vec<ConfigDirEntry>> {
        let ConfigPath::Project(path) = path else {
            return self.io_ops.read_dir(path).await;
        };
        let path = self.cell_resolver.get_cell_path(path);

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
) -> BoxFuture<'a, buck2_error::Result<()>> {
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
    use buck2_fs::fs_util::uncategorized as fs_util;
    use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
    use buck2_fs::paths::abs_path::AbsPath;

    use super::*;

    fn create_project_filesystem() -> ProjectRoot {
        #[cfg(not(windows))]
        let root_path = "/".to_owned();
        #[cfg(windows)]
        let root_path = "C:/".to_owned();
        ProjectRoot::new_unchecked(AbsNormPathBuf::try_from(root_path).unwrap())
    }

    #[test]
    fn dir_with_file() -> buck2_error::Result<()> {
        let mut v = vec![];
        let dir = tempfile::tempdir()?;
        let root = AbsPath::new(dir.path())?;
        let file = root.join("foo");
        fs_util::write(&file, "")?;

        let file = AbsPath::new(&file)?;
        let dir = AbsPath::new(dir.path())?;

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
    fn empty_dir() -> buck2_error::Result<()> {
        let mut v = vec![];
        let dir = tempfile::tempdir()?;
        let dir = AbsPath::new(dir.path())?;

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
    fn non_existent_dir() -> buck2_error::Result<()> {
        let mut v = vec![];
        let dir = tempfile::tempdir()?;
        let dir = dir.path().join("bad");
        let dir = AbsPath::new(&dir)?;

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
    fn dir_in_dir() -> buck2_error::Result<()> {
        let mut v = vec![];
        let dir = tempfile::tempdir()?;
        let dir = AbsPath::new(dir.path())?;
        fs_util::create_dir_all(dir.join("bad"))?;

        futures::executor::block_on(push_all_files_from_a_directory(
            &mut v,
            &ConfigPath::Global(AbsPath::new(dir)?.to_owned()),
            &mut DefaultConfigParserFileOps {
                project_fs: create_project_filesystem(),
            },
        ))?;
        assert_eq!(v, vec![]);

        Ok(())
    }

    #[test]
    fn file() -> buck2_error::Result<()> {
        let mut v = vec![];
        let file = tempfile::NamedTempFile::new()?;
        let file = AbsPath::new(file.path())?;

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
    fn dir_with_file_in_dir() -> buck2_error::Result<()> {
        let mut v = vec![];
        let dir = tempfile::tempdir()?;
        let dir = AbsPath::new(dir.path())?;
        let nested_dir = dir.join("nested");
        fs_util::create_dir_all(&nested_dir)?;
        let file = nested_dir.join("foo");
        fs_util::write(&file, "")?;

        let file = AbsPath::new(&file)?;
        let dir = AbsPath::new(&dir)?;

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
