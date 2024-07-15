/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::BufRead;
use std::path::PathBuf;

use anyhow::Context;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::project::ProjectRoot;
use futures::future::BoxFuture;
use futures::FutureExt;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct MainConfigFile {
    pub(crate) path: AbsNormPathBuf,

    /// if a main config file is in project or global
    pub(crate) owned_by_project: bool,
}

pub struct ConfigDirEntry {
    pub(crate) name: FileNameBuf,
    pub(crate) is_dir: bool,
}

#[async_trait::async_trait]
pub trait ConfigParserFileOps: Send + Sync {
    async fn read_file_lines(
        &mut self,
        path: &AbsNormPath,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Result<String, std::io::Error>> + Send>>;

    async fn file_exists(&mut self, path: &AbsNormPath) -> bool;

    async fn read_dir(&mut self, path: &AbsNormPath) -> anyhow::Result<Vec<ConfigDirEntry>>;
}

#[derive(buck2_error::Error, Debug)]
enum ReadDirError {
    #[error("Non-utf8 entry `{0}` in directory `{1}`")]
    NotUtf8(String, String),
}

pub(crate) struct DefaultConfigParserFileOps {
    #[allow(unused)] // TODO(JakobDegen): Use next diff
    pub(crate) project_fs: ProjectRoot,
}

#[async_trait::async_trait]
impl ConfigParserFileOps for DefaultConfigParserFileOps {
    async fn read_file_lines(
        &mut self,
        path: &AbsNormPath,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Result<String, std::io::Error>> + Send>> {
        let f = std::fs::File::open(path).with_context(|| format!("Reading file `{:?}`", path))?;
        let file = std::io::BufReader::new(f);
        Ok(Box::new(file.lines()))
    }

    async fn file_exists(&mut self, path: &AbsNormPath) -> bool {
        PathBuf::from(path.as_os_str()).exists()
    }

    async fn read_dir(&mut self, path: &AbsNormPath) -> anyhow::Result<Vec<ConfigDirEntry>> {
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

pub(crate) fn push_all_files_from_a_directory<'a>(
    buckconfig_paths: &'a mut Vec<MainConfigFile>,
    folder_path: &'a AbsNormPath,
    owned_by_project: bool,
    file_ops: &'a mut dyn ConfigParserFileOps,
) -> BoxFuture<'a, anyhow::Result<()>> {
    async move {
        for entry in file_ops.read_dir(folder_path).await? {
            let entry_path = folder_path.join(&entry.name);
            if entry.is_dir {
                push_all_files_from_a_directory(
                    buckconfig_paths,
                    &entry_path,
                    owned_by_project,
                    file_ops,
                )
                .await?;
            } else {
                buckconfig_paths.push(MainConfigFile {
                    path: entry_path,
                    owned_by_project,
                });
            }
        }

        Ok(())
    }
    .boxed()
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::fs_util;
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
            dir,
            false,
            &mut DefaultConfigParserFileOps {
                project_fs: create_project_filesystem(),
            },
        ))?;
        assert_eq!(
            v,
            vec![MainConfigFile {
                path: file.to_owned(),
                owned_by_project: false,
            }]
        );

        Ok(())
    }

    #[test]
    fn empty_dir() -> anyhow::Result<()> {
        let mut v = vec![];
        let dir = tempfile::tempdir()?;
        let dir = AbsNormPath::new(&dir)?;

        futures::executor::block_on(push_all_files_from_a_directory(
            &mut v,
            dir,
            false,
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
            dir,
            false,
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
            AbsNormPath::new(dir)?,
            false,
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
            file,
            false,
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
            dir,
            false,
            &mut DefaultConfigParserFileOps {
                project_fs: create_project_filesystem(),
            },
        ))?;
        assert_eq!(
            v,
            vec![MainConfigFile {
                path: file.to_owned(),
                owned_by_project: false,
            }]
        );

        Ok(())
    }
}
