/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_fs::paths::file_name::FileNameBuf;
use derive_more::Display;
use dice::DiceComputations;
use dice::EqualityBehavior;
use dice::Key;
use dice::OkPagableValueSerialize;
use dice::ValueSerialize;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use dupe::ResultDupedErrExt;
use pagable::Pagable;
use pagable::pagable_typetag;

use crate::buildfiles::HasBuildfiles;
use crate::file_ops::delegate::FileOpsDelegateWithIgnores;
use crate::file_ops::delegate::get_delegated_file_ops;
use crate::file_ops::error::FileReadError;
use crate::file_ops::error::extended_ignore_error;
use crate::file_ops::metadata::RawPathMetadata;
use crate::file_ops::metadata::ReadDirOutput;
use crate::ignores::file_ignores::FileIgnoreResult;
use crate::io::ReadDirError;

pub struct DiceFileComputations;

/// Functions for accessing files with keys on the dice graph.
impl DiceFileComputations {
    /// Filters out ignored paths
    pub async fn read_dir<'d>(
        ctx: &mut DiceComputations<'d>,
        path: CellPath,
    ) -> buck2_error::Result<&'d ReadDirOutput> {
        ctx.compute(&ReadDirKey {
            path,
            check_ignores: CheckIgnores::Yes,
        })
        .await?
        .as_ref()
        .duped_err()
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
        ctx.compute(&ExistsMatchingExactCaseKey(path)).await?.dupe()
    }

    pub async fn read_dir_include_ignores<'d>(
        ctx: &mut DiceComputations<'d>,
        path: CellPath,
    ) -> buck2_error::Result<&'d ReadDirOutput> {
        ctx.compute(&ReadDirKey {
            path,
            check_ignores: CheckIgnores::No,
        })
        .await?
        .as_ref()
        .duped_err()
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
        ctx.compute(&ReadFileKey(path))
            .await?
            .as_ref()
            .duped_err()?
            .read_file_if_exists(ctx)
            .await
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
        ctx.compute(&PathMetadataKey(path)).await?.dupe()
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
        get_delegated_file_ops(ctx, path.cell(), CheckIgnores::Yes)
            .await?
            .is_ignored(path.path())
            .await
    }

    pub async fn buildfiles<'d>(
        ctx: &mut DiceComputations<'d>,
        cell: CellName,
    ) -> buck2_error::Result<&'d Arc<[FileNameBuf]>> {
        ctx.get_buildfiles(cell).await
    }
}

#[derive(
    Debug, Display, Clone, Dupe, Copy, PartialEq, Eq, Hash, Allocative, Pagable
)]
pub(crate) enum CheckIgnores {
    Yes,
    No,
}

/// The return value of a `ReadFileKey` computation.
///
/// Instead of the actual file contents, this holds a `FileOpsDelegateWithIgnores`
/// and the path to read. The actual file read happens when the caller invokes
/// `read_file_if_exists` on this value. This ensures we don't store file
/// contents in memory.
#[derive(Clone, Dupe, Allocative, Pagable)]
pub struct ReadFileValue {
    file_ops: FileOpsDelegateWithIgnores,
    path: CellPath,
}

impl ReadFileValue {
    // FIXME: `ReadFileKey` is not marked as transient if this returns an error, which is
    // unfortunate.
    // A reasonable fix would be to take a dep on an always transient key in the error case.
    pub async fn read_file_if_exists(
        &self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Option<String>> {
        self.file_ops
            .read_file_if_exists(ctx, self.path.ipath())
            .await
    }
}

#[derive(
    Clone, Dupe, Copy, Display, Debug, Eq, Hash, PartialEq, Allocative, Pagable
)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct ReadFileKey(CellPath);

#[async_trait]
impl Key for ReadFileKey {
    type Value = buck2_error::Result<ReadFileValue>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let file_ops = get_delegated_file_ops(ctx, self.0.cell(), CheckIgnores::No).await?;
        Ok(ReadFileValue {
            file_ops,
            path: self.0.dupe(),
        })
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::AlwaysUnequal
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        OkPagableValueSerialize::<Self::Value>::new()
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative, Pagable)]
#[display("{}", path)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct ReadDirKey {
    path: CellPath,
    check_ignores: CheckIgnores,
}

#[async_trait]
impl Key for ReadDirKey {
    type Value = buck2_error::Result<ReadDirOutput>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let file_ops = get_delegated_file_ops(ctx, self.path.cell(), self.check_ignores).await?;
        file_ops.read_dir(ctx, self.path.ipath()).await
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        })
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        OkPagableValueSerialize::<Self::Value>::new()
    }
}

#[derive(Clone, Display, Allocative, Debug, Eq, Hash, PartialEq, Pagable)]
#[display("{}", _0)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct ExistsMatchingExactCaseKey(CellPath);

#[async_trait]
impl Key for ExistsMatchingExactCaseKey {
    type Value = buck2_error::Result<bool>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        get_delegated_file_ops(ctx, self.0.cell(), CheckIgnores::Yes)
            .await?
            .exists_matching_exact_case(self.0.ipath(), ctx)
            .await
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        })
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        OkPagableValueSerialize::<Self::Value>::new()
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative, Pagable)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct PathMetadataKey(CellPath);

#[async_trait]
impl Key for PathMetadataKey {
    type Value = buck2_error::Result<Option<RawPathMetadata>>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let res = get_delegated_file_ops(ctx, self.0.cell(), CheckIgnores::No)
            .await?
            .read_path_metadata_if_exists(ctx, self.0.ipath())
            .await?;

        Ok(res)
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        })
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        OkPagableValueSerialize::<Self::Value>::new()
    }
}

/// out-of-line impl for DiceComputations::read_dir_ext so it doesn't add noise to the api
async fn read_dir_ext(
    ctx: &mut DiceComputations<'_>,
    path: CellPath,
) -> Result<ReadDirOutput, ReadDirError> {
    match DiceFileComputations::read_dir(ctx, path).await {
        Ok(v) => Ok(v.dupe()),
        Err(e) => match extended_ignore_error(ctx, path).await {
            Some(e) => Err(e),
            None => Err(e.into()),
        },
    }
}
