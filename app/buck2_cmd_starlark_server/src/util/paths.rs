/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::ops::Deref;

use async_recursion::async_recursion;
use buck2_client_ctx::path_arg::PathArg;
use buck2_common::file_ops::dice::DiceFileComputations;
use buck2_common::file_ops::metadata::FileType;
use buck2_common::file_ops::metadata::RawPathMetadata;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bxl::BxlFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::package::PackageLabel;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_interpreter::paths::path::OwnedStarlarkPath;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use dice::DiceComputations;
use dupe::Dupe;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum StarlarkFilesError {
    #[error("File not found, `{0}`")]
    FileNotFound(CellPath),
    #[error("Symlinks and other esoteric files are not supported, `{0}`")]
    UnsupportedFileType(CellPath),
}

#[async_recursion]
async fn starlark_file(
    ctx: &mut DiceComputations<'_>,
    cell_path: CellPath,
    // None = this file was given explicitly
    // Some = it was a directory traversal (and we know its type)
    recursive: Option<FileType>,
    files: &mut Vec<OwnedStarlarkPath>,
) -> buck2_error::Result<()> {
    if recursive.is_some()
        && DiceFileComputations::is_ignored(ctx, cell_path.as_ref())
            .await?
            .is_ignored()
    {
        // File is ignored by Buck, give up on it
        return Ok(());
    }

    let typ = match &recursive {
        Some(typ) => typ.dupe(),
        None => {
            match DiceFileComputations::read_path_metadata_if_exists(ctx, cell_path.as_ref())
                .await?
            {
                None => {
                    return Err(StarlarkFilesError::FileNotFound(cell_path).into());
                }
                Some(RawPathMetadata::Directory) => FileType::Directory,
                Some(RawPathMetadata::File(_)) => FileType::File,
                Some(RawPathMetadata::Symlink { .. }) => FileType::Symlink,
            }
        }
    };

    match typ {
        FileType::Directory => {
            for entry in DiceFileComputations::read_dir(ctx, cell_path.as_ref())
                .await?
                .included
                .iter()
            {
                let child_path = cell_path.join(&entry.file_name);
                starlark_file(ctx, child_path, Some(entry.file_type.dupe()), files).await?;
            }
        }
        FileType::File => {
            let is_buildfile = match cell_path.path().file_name() {
                None => false,
                Some(file_name) => DiceFileComputations::buildfiles(ctx, cell_path.cell())
                    .await?
                    .iter()
                    .any(|x| (*x).deref() == file_name),
            };

            if is_buildfile {
                files.push(OwnedStarlarkPath::BuildFile(BuildFilePath::new(
                    PackageLabel::from_cell_path(cell_path.parent().unwrap())?,
                    cell_path.path().file_name().unwrap().to_owned(),
                )));
            } else if cell_path.path().as_str().ends_with(".bxl") {
                files.push(OwnedStarlarkPath::BxlFile(BxlFilePath::new(cell_path)?));
            } else if let Some(path) = PackageFilePath::from_file_path(cell_path.as_ref()) {
                files.push(OwnedStarlarkPath::PackageFile(path));
            } else if recursive.is_none() || cell_path.path().as_str().ends_with(".bzl") {
                // If a file was asked for explicitly, and is nothing else, treat it as .bzl file
                // If it's not explicit, just ignore it (probably a source file)
                files.push(OwnedStarlarkPath::LoadFile(ImportPath::new_same_cell(
                    cell_path,
                )?));
            }
        }
        FileType::Symlink | FileType::Unknown => {
            if recursive.is_none() {
                return Err(StarlarkFilesError::UnsupportedFileType(cell_path).into());
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
) -> buck2_error::Result<Vec<OwnedStarlarkPath>> {
    let mut files = Vec::new();

    for path in paths {
        let path = path.resolve(context.working_dir_abs());
        let cell_path = cell_resolver.get_cell_path_from_abs_path(&path, context.project_root())?;
        starlark_file(ctx, cell_path, None, &mut files).await?;
    }
    Ok(files)
}
