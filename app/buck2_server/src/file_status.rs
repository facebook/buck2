/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;
use std::path::PathBuf;

use async_recursion::async_recursion;
use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileOps;
use buck2_common::file_ops::RawPathMetadata;
use buck2_common::file_ops::RawSymlink;
use buck2_core::cells::CellResolver;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::raw_output::RawOuputGuard;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;
use itertools::Itertools;
use thiserror::Error;

use crate::ctx::ServerCommandContext;

pub(crate) async fn file_status_command(
    ctx: ServerCommandContext,
    req: buck2_cli_proto::FileStatusRequest,
) -> anyhow::Result<buck2_cli_proto::GenericResponse> {
    run_server_command(FileStatusServerCommand { req }, box ctx).await
}
struct FileStatusServerCommand {
    req: buck2_cli_proto::FileStatusRequest,
}

struct FileStatusResult<'a> {
    /// Number of things we check
    checked: usize,
    /// Number of ones that were bad
    bad: usize,
    /// Handle for writing output
    stderr: RawOuputGuard<'a>,
}

impl FileStatusResult<'_> {
    fn checking(&mut self) {
        self.checked += 1;
    }

    fn mismatch(&mut self, err: Mismatch) -> anyhow::Result<()> {
        writeln!(self.stderr, "MISMATCH: {}", err)?;
        self.bad += 1;
        Ok(())
    }
}

#[async_trait]
impl ServerCommandTemplate for FileStatusServerCommand {
    type StartEvent = buck2_data::FileStatusCommandStart;
    type EndEvent = buck2_data::FileStatusCommandEnd;
    type Response = buck2_cli_proto::GenericResponse;

    async fn command<'v>(
        &self,
        server_ctx: &'v dyn ServerCommandContextTrait,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        let file_ops = ctx.file_ops();
        let cell_resolver = ctx.get_cell_resolver().await?;
        let project_root = server_ctx.project_root();
        let mut result = FileStatusResult {
            checked: 0,
            bad: 0,
            stderr: server_ctx.stderr()?,
        };

        for path in &self.req.paths {
            let path = ProjectRelativePath::new(path)?;
            writeln!(result.stderr, "Check file status: {}", path)?;
            check_file_status(&file_ops, &cell_resolver, project_root, path, &mut result).await?;
        }
        if result.bad != 0 {
            Err(anyhow::anyhow!("Failed with {} mismatches", result.bad))
        } else {
            writeln!(
                result.stderr,
                "No mismatches detected ({} entries checked)",
                result.checked
            )?;
            Ok(buck2_cli_proto::GenericResponse {})
        }
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }
}

#[derive(Debug, Error)]
enum Mismatch {
    #[error("Existence of {0}, fs={1}, dice={2}")]
    Existence(ProjectRelativePathBuf, bool, bool),
    #[error("Symlink destination of {0}, fs={1}, dice={2}")]
    SymlinkTarget(ProjectRelativePathBuf, PathBuf, PathBuf),
    #[error("Entry {0} disagrees on the type, dice={1}")]
    FileType(ProjectRelativePathBuf, &'static str),
    #[error("Directory {0} disagrees on the contents, fs={}, dice={}", display_filenames(.1), display_filenames(.2))]
    DirContents(ProjectRelativePathBuf, Vec<FileNameBuf>, Vec<FileNameBuf>),
    #[error("File {0} disagrees on file size, fs={1}, dice={2}")]
    FileSize(ProjectRelativePathBuf, u64, u64),
    #[error("File {0} disagrees on file digest, fs={1}, dice={2}")]
    FileDigest(ProjectRelativePathBuf, FileDigest, FileDigest),
}

fn display_filenames(files: &[FileNameBuf]) -> String {
    files.iter().map(|x| x.as_str()).join(",")
}

#[async_recursion]
async fn check_file_status(
    file_ops: &dyn FileOps,
    cell_resolver: &CellResolver,
    project_root: &ProjectRoot,
    path: &ProjectRelativePath,
    result: &mut FileStatusResult,
) -> anyhow::Result<()> {
    result.checking();

    let cell_path = cell_resolver.get_cell_path(path)?;
    if file_ops.is_ignored(&cell_path).await? {
        return Ok(());
    }

    let abs_path = project_root.resolve(path);
    let fs_metadata = fs_util::symlink_metadata_if_exists(&abs_path)?;
    let dice_metadata = file_ops.read_path_metadata_if_exists(&cell_path).await?;

    match (&fs_metadata, &dice_metadata) {
        (None, None) => {}
        (Some(fs_metadata), Some(dice_metadata)) => match dice_metadata {
            RawPathMetadata::Symlink { at: _, to: dice_to } => {
                if !fs_metadata.is_symlink() {
                    result.mismatch(Mismatch::FileType(path.to_owned(), "symlink"))?;
                } else {
                    // Canonicalize isn't quite right here, but it's close enough
                    // given we encourage to have very few symlinks.
                    // Some paths have broken symlinks, in which case canonicalize fails,
                    // so we skip if we get a failure.
                    if let Ok(fs_to) = fs_util::canonicalize(&abs_path) {
                        let dice_to = match dice_to {
                            RawSymlink::Relative(x) => project_root
                                .resolve(&cell_resolver.resolve_path(x)?)
                                .as_path()
                                .to_owned(),
                            RawSymlink::External(x) => x.to_path_buf(),
                        };
                        if fs_to != dice_to.as_path() {
                            result.mismatch(Mismatch::SymlinkTarget(
                                path.to_owned(),
                                fs_to,
                                dice_to,
                            ))?;
                        }
                    }
                }
            }
            RawPathMetadata::File(dice_metadata) => {
                if !fs_metadata.is_file() {
                    result.mismatch(Mismatch::FileType(path.to_owned(), "file"))?;
                } else {
                    // We don't check is_executable as there are multiple definitions of this,
                    // and it usually isn't too important.
                    if fs_metadata.len() != dice_metadata.digest.size() {
                        result.mismatch(Mismatch::FileSize(
                            path.to_owned(),
                            fs_metadata.len(),
                            dice_metadata.digest.size(),
                        ))?;
                    } else {
                        let fs_digest = FileDigest::from_file_disk(&abs_path)?;
                        if &fs_digest != dice_metadata.digest.data() {
                            result.mismatch(Mismatch::FileDigest(
                                path.to_owned(),
                                fs_digest,
                                dice_metadata.digest.data().to_owned(),
                            ))?;
                        }
                    }
                }
            }
            RawPathMetadata::Directory => {
                if !fs_metadata.is_dir() {
                    result.mismatch(Mismatch::FileType(path.to_owned(), "directory"))?;
                } else {
                    let mut fs_list = Vec::new();
                    for entry in fs_util::read_dir(&abs_path)? {
                        fs_list.push(FileNameBuf::try_from(
                            entry?.file_name().into_string().unwrap(),
                        )?);
                    }
                    let dice_read_dir = file_ops.read_dir_with_ignores(&cell_path).await?;
                    let mut dice_list: Vec<_> = dice_read_dir
                        .included
                        .iter()
                        .chain(dice_read_dir.ignored.iter())
                        .map(|x| x.file_name.clone())
                        .collect();
                    fs_list.sort();
                    dice_list.sort();
                    if fs_list != dice_list {
                        result.mismatch(Mismatch::DirContents(
                            path.to_owned(),
                            fs_list,
                            dice_list,
                        ))?;
                    } else {
                        for file in dice_list {
                            let mut path = path.to_owned();
                            path.push(file);
                            check_file_status(file_ops, cell_resolver, project_root, &path, result)
                                .await?;
                        }
                    }
                }
            }
        },
        _ => result.mismatch(Mismatch::Existence(
            path.to_owned(),
            fs_metadata.is_some(),
            dice_metadata.is_some(),
        ))?,
    }
    Ok(())
}
