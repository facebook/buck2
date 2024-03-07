/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Deref;

use async_recursion::async_recursion;
use buck2_client_ctx::path_arg::PathArg;
use buck2_common::dice::file_ops::DiceFileComputations;
use buck2_common::file_ops::FileType;
use buck2_common::file_ops::RawPathMetadata;
use buck2_common::io::IoProvider;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_interpreter::paths::bxl::BxlFilePath;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_interpreter::paths::path::OwnedStarlarkPath;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use dice::DiceComputations;
use dupe::Dupe;

#[derive(Debug, buck2_error::Error)]
enum StarlarkFilesError {
    #[error("File not found, `{0}`")]
    FileNotFound(ProjectRelativePathBuf),
    #[error("Symlinks and other esoteric files are not supported, `{0}`")]
    UnsupportedFileType(ProjectRelativePathBuf),
}

#[async_recursion]
async fn starlark_file(
    ctx: &mut DiceComputations<'_>,
    proj_path: ProjectRelativePathBuf,
    // None = this file was given explicitly
    // Some = it was a directory traversal (and we know its type)
    recursive: Option<FileType>,
    cell_resolver: &CellResolver,
    io: &dyn IoProvider,
    files: &mut Vec<OwnedStarlarkPath>,
) -> anyhow::Result<()> {
    let cell_path = cell_resolver.get_cell_path(&proj_path)?;
    if recursive.is_some() && DiceFileComputations::is_ignored(ctx, cell_path.as_ref()).await? {
        // File is ignored by Buck, give up on it
        return Ok(());
    }

    let typ = match &recursive {
        Some(typ) => typ.dupe(),
        None => match io.read_path_metadata_if_exists(proj_path.clone()).await? {
            None => {
                return Err(StarlarkFilesError::FileNotFound(proj_path).into());
            }
            Some(RawPathMetadata::Directory) => FileType::Directory,
            Some(RawPathMetadata::File(_)) => {
                // It's a shame we throw away the digest we calculated, but not a huge deal (its cheap compared to parsing)
                FileType::File
            }
            Some(RawPathMetadata::Symlink { .. }) => FileType::Symlink,
        },
    };

    match typ {
        FileType::Directory => {
            for x in io.read_dir(proj_path.clone()).await? {
                let Ok(file_name) = FileName::new(&x.file_name) else {
                    // Skip files which buck does not like:
                    // this function works with `CellPath` values,
                    // which cannot be constructed from paths not acceptable by buck.
                    continue;
                };
                let mut child_path = proj_path.clone();
                child_path.push(file_name);
                starlark_file(ctx, child_path, Some(x.file_type), cell_resolver, io, files).await?;
            }
        }
        FileType::File => {
            // It's a shame we throw away the digest we calculated, but not a huge deal (its cheap compared to parsing)
            let is_buildfile = match proj_path.file_name() {
                None => false,
                Some(file_name) => DiceFileComputations::buildfiles(ctx, cell_path.cell())
                    .await?
                    .iter()
                    .any(|x| (*x).deref() == file_name),
            };

            if is_buildfile {
                files.push(OwnedStarlarkPath::BuildFile(BuildFilePath::new(
                    PackageLabel::from_cell_path(cell_path.parent().unwrap()),
                    proj_path.file_name().unwrap().to_owned(),
                )));
            } else if proj_path.as_str().ends_with(".bxl") {
                files.push(OwnedStarlarkPath::BxlFile(BxlFilePath::new(cell_path)?));
            } else if proj_path.ends_with(PackageFilePath::PACKAGE_FILE_NAME) {
                // `parent` must return `Some` if we have a non-empty file, which we do because of above
                files.push(OwnedStarlarkPath::PackageFile(PackageFilePath::for_dir(
                    cell_path.parent().unwrap(),
                )))
            } else if recursive.is_none() || proj_path.as_str().ends_with(".bzl") {
                // If a file was asked for explicitly, and is nothing else, treat it as .bzl file
                // If it's not explicit, just ignore it (probably a source file)
                files.push(OwnedStarlarkPath::LoadFile(ImportPath::new_same_cell(
                    cell_path,
                )?));
            }
        }
        FileType::Symlink | FileType::Unknown => {
            if recursive.is_none() {
                return Err(StarlarkFilesError::UnsupportedFileType(proj_path).into());
            }
        }
    }
    Ok(())
}

/// Find the paths to apply Starlark to (e.g. linter, typecheck)
pub(crate) async fn starlark_files(
    ctx: &mut DiceComputations<'_>,
    paths: &[PathArg],
    context: &dyn ServerCommandContextTrait,
    cell_resolver: &CellResolver,
    io: &dyn IoProvider,
) -> anyhow::Result<Vec<OwnedStarlarkPath>> {
    let mut files = Vec::new();

    for path in paths {
        let path = path.resolve(context.working_dir_abs());
        let cell_path = cell_resolver.get_cell_path_from_abs_path(&path, context.project_root())?;
        let proj_path = cell_resolver.resolve_path(cell_path.as_ref())?;
        starlark_file(ctx, proj_path, None, cell_resolver, io, &mut files).await?;
    }
    Ok(files)
}
