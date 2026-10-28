/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::file_name::FileNameBuf;
use buck2_util::arc_str::PackedStringSlice;
use buck2_util::arc_str::PackedStringSliceBuilder;
use dice::DiceComputations;
use dupe::Dupe;

use crate::buildfiles::HasBuildfiles;
use crate::dice::cells::HasCellResolver;
use crate::file_ops::delegate::external_cell_delegate;
use crate::file_ops::error::FileReadError;
use crate::file_ops::error::extended_ignore_error;
use crate::file_ops::io;
use crate::file_ops::metadata::FileType;
use crate::file_ops::metadata::RawPathMetadata;
use crate::file_ops::metadata::ReadDirOutput;
use crate::ignores::file_ignores::CellFileIgnores;
use crate::ignores::file_ignores::FileIgnoreResult;
use crate::ignores::ignore_set::HasIgnoreSet;
use crate::io::ReadDirError;

pub struct DiceFileComputations;

/// Functions for accessing files with keys on the dice graph.
impl DiceFileComputations {
    /// Filters out ignored paths
    pub async fn read_dir(
        ctx: &mut DiceComputations<'_>,
        path: CellPath,
    ) -> buck2_error::Result<ReadDirOutput> {
        read_dir_impl(ctx, path, true).await
    }

    /// Returns if a directory or file exists at the given path, but checks for an exact,
    /// case-sensitive match.
    ///
    /// Note that case-sensitive match is only done on the last element of the path, not any of the
    /// elements before.
    pub async fn exists_matching_exact_case(
        ctx: &mut DiceComputations<'_>,
        path: CellPath,
    ) -> buck2_error::Result<bool> {
        let cells = ctx.get_cell_resolver().await?;
        if cell_ignores(ctx, cells, path.cell())?
            .check(path.path())
            .is_ignored()
        {
            return Ok(false);
        }
        match external_cell_delegate(ctx, cells, path.cell()).await? {
            Some(delegate) => delegate.exists_matching_exact_case(ctx, path.ipath()).await,
            None => io::exists_matching_exact_case(ctx, cells.resolve_ipath(path)?).await,
        }
    }

    pub async fn read_dir_include_ignores(
        ctx: &mut DiceComputations<'_>,
        path: CellPath,
    ) -> buck2_error::Result<ReadDirOutput> {
        read_dir_impl(ctx, path, false).await
    }

    /// Like read_dir, but with extended error information. This may add additional dice dependencies.
    pub async fn read_dir_ext(
        ctx: &mut DiceComputations<'_>,
        path: CellPath,
    ) -> Result<ReadDirOutput, ReadDirError> {
        read_dir_ext(ctx, path).await
    }

    /// Does not check if the path is ignored
    ///
    /// TODO(cjhopman): error on ignored paths, maybe.
    pub async fn read_file_if_exists(
        ctx: &mut DiceComputations<'_>,
        path: CellPath,
    ) -> buck2_error::Result<Option<String>> {
        let cells = ctx.get_cell_resolver().await?;
        match external_cell_delegate(ctx, cells, path.cell()).await? {
            Some(delegate) => delegate.read_file_if_exists(ctx, path.ipath()).await,
            None => io::read_file_if_exists(ctx, cells.resolve_ipath(path)?).await,
        }
    }

    /// Does not check if the path is ignored
    pub async fn read_file(
        ctx: &mut DiceComputations<'_>,
        path: CellPath,
    ) -> Result<String, FileReadError> {
        match Self::read_file_if_exists(ctx, path).await {
            Ok(result) => result.ok_or_else(|| FileReadError::NotFound(path.to_string())),
            Err(e) => Err(FileReadError::Buck(e)),
        }
    }

    /// Does not check if the path is ignored
    pub async fn read_path_metadata_if_exists(
        ctx: &mut DiceComputations<'_>,
        path: CellPath,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        let cells = ctx.get_cell_resolver().await?;
        match external_cell_delegate(ctx, cells, path.cell()).await? {
            Some(delegate) => {
                delegate
                    .read_path_metadata_if_exists(ctx, path.ipath())
                    .await
            }
            None => Ok(
                io::read_path_metadata_if_exists(ctx, cells.resolve_ipath(path)?)
                    .await?
                    .map(|meta| meta.map(|p| cells.get_cell_path(&p))),
            ),
        }
    }

    /// Does not check if the path is ignored
    pub async fn read_path_metadata(
        ctx: &mut DiceComputations<'_>,
        path: CellPath,
    ) -> Result<RawPathMetadata, FileReadError> {
        match Self::read_path_metadata_if_exists(ctx, path).await {
            Ok(result) => result.ok_or_else(|| FileReadError::NotFound(path.to_string())),
            Err(e) => Err(FileReadError::Buck(e)),
        }
    }

    pub async fn is_ignored(
        ctx: &mut DiceComputations<'_>,
        path: CellPath,
    ) -> buck2_error::Result<FileIgnoreResult> {
        let cells = ctx.get_cell_resolver().await?;
        Ok(cell_ignores(ctx, cells, path.cell())?.check(path.path()))
    }

    pub async fn buildfiles<'d>(
        ctx: &mut DiceComputations<'d>,
        cell: CellName,
    ) -> buck2_error::Result<&'d Arc<[FileNameBuf]>> {
        ctx.get_buildfiles(cell).await
    }
}

fn cell_ignores(
    ctx: &DiceComputations<'_>,
    cells: &CellResolver,
    cell: CellName,
) -> buck2_error::Result<CellFileIgnores> {
    cells.get(cell)?;
    Ok(CellFileIgnores::new(
        cells.dupe(),
        cell,
        ctx.global_data().get_ignore_set(),
    ))
}

async fn read_dir_impl(
    ctx: &mut DiceComputations<'_>,
    path: CellPath,
    check_ignores: bool,
) -> buck2_error::Result<ReadDirOutput> {
    let cells = ctx.get_cell_resolver().await?;
    let ignores = if check_ignores {
        let ignores = cell_ignores(ctx, cells, path.cell())?;
        // TODO(cjhopman): This should also probably verify that the parent chain is not ignored.
        ignores.check(path.path()).into_result()?;
        Some(ignores)
    } else {
        None
    };

    let entries = match external_cell_delegate(ctx, cells, path.cell()).await? {
        Some(delegate) => delegate.read_dir(ctx, path.ipath()).await?,
        None => io::read_dir(ctx, cells.resolve_ipath(path)?).await?,
    };

    let included = match ignores {
        Some(ignores) => without_ignored(path, entries, &ignores),
        None => entries,
    };
    Ok(ReadDirOutput { included })
}

/// `entries`, listed at `path`, minus those the ignores apply to.
fn without_ignored(
    path: CellPath,
    entries: PackedStringSlice<FileName, FileType>,
    ignores: &CellFileIgnores,
) -> PackedStringSlice<FileName, FileType> {
    let path = path.path();
    let is_ignored = |file_name: &str| {
        let mut cell_relative_path_buf;
        let cell_relative_path: &str = if path.is_empty() {
            file_name
        } else {
            cell_relative_path_buf =
                String::with_capacity(path.as_str().len() + 1 + file_name.len());
            cell_relative_path_buf.push_str(path.as_str());
            cell_relative_path_buf.push('/');
            cell_relative_path_buf.push_str(file_name);
            &cell_relative_path_buf
        };
        ignores
            .check(CellRelativePath::unchecked_new(cell_relative_path))
            .is_ignored()
    };

    // Input is sorted; we only remove entries, never reorder, so the output stays sorted. Defer
    // allocating a builder until we encounter the first ignored entry, so that the common
    // no-ignored-files case can reuse the existing alloc.
    let mut builder: Option<PackedStringSliceBuilder<FileName, FileType>> = None;
    for (idx, (file_name, file_type)) in entries.iter_with_tag().enumerate() {
        if is_ignored(file_name.as_str()) {
            builder.get_or_insert_with(|| {
                let mut b = PackedStringSliceBuilder::with_capacity(entries.len() - 1);
                for (fname, ftype) in entries.iter_with_tag().take(idx) {
                    b.push(fname, ftype);
                }
                b
            });
            continue;
        }
        if let Some(b) = builder.as_mut() {
            b.push(file_name, file_type);
        }
    }
    match builder {
        Some(b) => b.build_sorted(),
        None => entries,
    }
}

/// out-of-line impl for DiceComputations::read_dir_ext so it doesn't add noise to the api
async fn read_dir_ext(
    ctx: &mut DiceComputations<'_>,
    path: CellPath,
) -> Result<ReadDirOutput, ReadDirError> {
    match DiceFileComputations::read_dir(ctx, path).await {
        Ok(v) => Ok(v),
        Err(e) => match extended_ignore_error(ctx, path).await {
            Some(e) => Err(e),
            None => Err(e.into()),
        },
    }
}
