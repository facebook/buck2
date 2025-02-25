/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_futures::cancellation::CancellationContext;
use cmp_any::PartialEqAny;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceTransactionUpdater;
use dice::InvalidationSourcePriority;
use dice::Key;
use dice::LinearRecomputeDiceComputations;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::FutureExt;

use crate::buildfiles::HasBuildfiles;
use crate::dice::file_ops::delegate::get_delegated_file_ops;
use crate::file_ops::FileOps;
use crate::file_ops::FileOpsError;
use crate::file_ops::RawPathMetadata;
use crate::file_ops::ReadDirOutput;
use crate::ignores::file_ignores::FileIgnoreResult;
use crate::io::ReadDirError;

pub mod delegate;

/// A wrapper around DiceComputations for places that want to interact with a dyn FileOps.
///
/// In general, it's better to use DiceFileComputations directly.
pub struct DiceFileOps<'c, 'd>(pub &'c LinearRecomputeDiceComputations<'d>);

pub struct DiceFileComputations;

/// Functions for accessing files with keys on the dice graph.
impl DiceFileComputations {
    /// Filters out ignored paths
    pub async fn read_dir(
        ctx: &mut DiceComputations<'_>,
        path: CellPathRef<'_>,
    ) -> buck2_error::Result<ReadDirOutput> {
        ctx.compute(&ReadDirKey {
            path: path.to_owned(),
            check_ignores: CheckIgnores::Yes,
        })
        .await?
    }

    pub async fn read_dir_include_ignores(
        ctx: &mut DiceComputations<'_>,
        path: CellPathRef<'_>,
    ) -> buck2_error::Result<ReadDirOutput> {
        ctx.compute(&ReadDirKey {
            path: path.to_owned(),
            check_ignores: CheckIgnores::No,
        })
        .await?
    }

    /// Like read_dir, but with extended error information. This may add additional dice dependencies.
    pub async fn read_dir_ext(
        ctx: &mut DiceComputations<'_>,
        path: CellPathRef<'_>,
    ) -> Result<ReadDirOutput, ReadDirError> {
        read_dir_ext(ctx, path).await
    }

    /// Does not check if the path is ignored
    ///
    /// TODO(cjhopman): error on ignored paths, maybe.
    pub async fn read_file_if_exists(
        ctx: &mut DiceComputations<'_>,
        path: CellPathRef<'_>,
    ) -> buck2_error::Result<Option<String>> {
        let file_ops = get_delegated_file_ops(ctx, path.cell(), CheckIgnores::No).await?;
        let () = ctx.compute(&ReadFileKey(Arc::new(path.to_owned()))).await?;
        // FIXME(JakobDegen): We intentionally avoid storing the result of this function in dice.
        // However, that also means that the `ReadFileKey` is not marked as transient if this
        // returns an error, which is unfortunate.
        file_ops.read_file_if_exists(path.path()).await
    }

    /// Does not check if the path is ignored
    pub async fn read_file(
        ctx: &mut DiceComputations<'_>,
        path: CellPathRef<'_>,
    ) -> buck2_error::Result<String> {
        Self::read_file_if_exists(ctx, path)
            .await?
            .ok_or_else(|| FileOpsError::FileNotFound(path.to_string()).into())
    }

    /// Does not check if the path is ignored
    pub async fn read_path_metadata_if_exists(
        ctx: &mut DiceComputations<'_>,
        path: CellPathRef<'_>,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        ctx.compute(&PathMetadataKey(path.to_owned())).await?
    }

    /// Does not check if the path is ignored
    pub async fn read_path_metadata(
        ctx: &mut DiceComputations<'_>,
        path: CellPathRef<'_>,
    ) -> buck2_error::Result<RawPathMetadata> {
        Self::read_path_metadata_if_exists(ctx, path)
            .await?
            .ok_or_else(|| FileOpsError::FileNotFound(path.to_string()).into())
    }

    pub async fn is_ignored(
        ctx: &mut DiceComputations<'_>,
        path: CellPathRef<'_>,
    ) -> buck2_error::Result<FileIgnoreResult> {
        get_delegated_file_ops(ctx, path.cell(), CheckIgnores::Yes)
            .await?
            .is_ignored(path.path())
            .await
    }

    pub async fn buildfiles<'a>(
        ctx: &mut DiceComputations<'_>,
        cell: CellName,
    ) -> buck2_error::Result<Arc<[FileNameBuf]>> {
        ctx.get_buildfiles(cell).await
    }
}

#[derive(Debug, Display, Clone, Dupe, Copy, PartialEq, Eq, Hash, Allocative)]
pub(crate) enum CheckIgnores {
    Yes,
    No,
}

#[derive(Allocative)]
pub struct FileChangeTracker {
    files_to_dirty: HashSet<ReadFileKey>,
    dirs_to_dirty: HashSet<ReadDirKey>,
    paths_to_dirty: HashSet<PathMetadataKey>,

    /// Normally, we ignore directory modification events from file watchers and instead compute
    /// them ourselves when a file in the directory is reported as having been added or removed.
    /// However, watchman has a bug in which it sometimes incorrectly doesn't report files as having
    /// been added/removed. We work around this by implementing some logic that marks a directory
    /// listing as being invalid if both the directory and at least one of its entries is reported
    /// as having been modified.
    ///
    /// We cannot unconditionally respect directory modification events from the file watcher, as it
    /// is not aware of our ignore rules.
    maybe_modified_dirs: HashSet<CellPath>,
}

impl FileChangeTracker {
    pub fn new() -> Self {
        Self {
            files_to_dirty: Default::default(),
            dirs_to_dirty: Default::default(),
            paths_to_dirty: Default::default(),
            maybe_modified_dirs: Default::default(),
        }
    }

    pub fn write_to_dice(mut self, ctx: &mut DiceTransactionUpdater) -> buck2_error::Result<()> {
        // See comment on `maybe_modified_dirs`
        for p in self.paths_to_dirty.clone() {
            if let Some(dir) = p.0.parent() {
                if self.maybe_modified_dirs.contains(&dir.to_owned()) {
                    self.insert_dir_keys(dir.to_owned());
                }
            }
        }

        ctx.changed(self.files_to_dirty)?;
        ctx.changed(self.dirs_to_dirty)?;
        ctx.changed(self.paths_to_dirty)?;

        Ok(())
    }

    fn file_contents_modify(&mut self, path: CellPath) {
        self.files_to_dirty
            .insert(ReadFileKey(Arc::new(path.clone())));
        self.paths_to_dirty.insert(PathMetadataKey(path));
    }

    fn insert_dir_keys(&mut self, path: CellPath) {
        self.dirs_to_dirty.insert(ReadDirKey {
            path: path.clone(),
            check_ignores: CheckIgnores::No,
        });
        self.dirs_to_dirty.insert(ReadDirKey {
            path,
            check_ignores: CheckIgnores::Yes,
        });
    }

    pub fn file_added_or_removed(&mut self, path: CellPath) {
        let parent = path.parent();

        self.file_contents_modify(path.clone());
        if let Some(parent) = parent {
            // The above can be None (validly!) if we have a cell we either create or delete.
            // That never happens in established repos, but if you are setting one up, it's not uncommon.
            // Since we don't include paths in different cells, the fact we don't dirty the parent
            // (which is in an enclosing cell) doesn't matter.
            self.insert_dir_keys(parent.to_owned());
        }
    }

    pub fn dir_added_or_removed(&mut self, path: CellPath) {
        self.paths_to_dirty.insert(PathMetadataKey(path.clone()));
        if let Some(parent) = path.parent() {
            let parent = parent.to_owned();
            // The above can be None (validly!) if we have a cell we either create or delete.
            // That never happens in established repos, but if you are setting one up, it's not uncommon.
            // Since we don't include paths in different cells, the fact we don't dirty the parent
            // (which is in an enclosing cell) doesn't matter.
            self.insert_dir_keys(parent);
        }
    }

    pub fn file_changed(&mut self, path: CellPath) {
        self.file_contents_modify(path)
    }

    pub fn file_removed(&mut self, path: CellPath) {
        self.file_added_or_removed(path)
    }

    pub fn file_added(&mut self, path: CellPath) {
        self.file_added_or_removed(path)
    }

    pub fn dir_changed(&mut self, path: CellPath) {
        self.paths_to_dirty.insert(PathMetadataKey(path.clone()));
        self.insert_dir_keys(path);
    }

    pub fn dir_maybe_changed(&mut self, path: CellPath) {
        self.maybe_modified_dirs.insert(path);
    }

    pub fn dir_added(&mut self, path: CellPath) {
        self.dir_added_or_removed(path)
    }

    pub fn dir_removed(&mut self, path: CellPath) {
        self.dir_added_or_removed(path)
    }
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
struct ReadFileKey(Arc<CellPath>);

#[async_trait]
impl Key for ReadFileKey {
    type Value = ();
    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        false
    }

    fn invalidation_source_priority() -> InvalidationSourcePriority {
        InvalidationSourcePriority::High
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display("{}", path)]
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
        get_delegated_file_ops(ctx, self.path.cell(), self.check_ignores)
            .await?
            .read_dir(self.path.as_ref().path())
            .await
            .map_err(buck2_error::Error::from)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }

    fn validity(x: &Self::Value) -> bool {
        x.is_ok()
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
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
            .read_path_metadata_if_exists(self.0.as_ref().path())
            .await?;

        match res {
            Some(RawPathMetadata::Symlink {
                at: ref path,
                to: _,
            }) => {
                ctx.compute(&ReadFileKey(path.dupe())).await?;
            }
            _ => (),
        };

        Ok(res)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }

    fn validity(x: &Self::Value) -> bool {
        x.is_ok()
    }

    fn invalidation_source_priority() -> InvalidationSourcePriority {
        InvalidationSourcePriority::High
    }
}

#[async_trait]
impl FileOps for DiceFileOps<'_, '_> {
    async fn read_file_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Option<String>> {
        DiceFileComputations::read_file_if_exists(&mut self.0.get(), path).await
    }

    async fn read_dir(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<ReadDirOutput> {
        DiceFileComputations::read_dir(&mut self.0.get(), path).await
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        DiceFileComputations::read_path_metadata_if_exists(&mut self.0.get(), path).await
    }

    async fn is_ignored(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<FileIgnoreResult> {
        DiceFileComputations::is_ignored(&mut self.0.get(), path).await
    }

    fn eq_token(&self) -> PartialEqAny {
        // We do not store this on DICE, so we don't care about equality.
        // Also we cannot do `PartialEqAny` here because `Self` is not `'static`.
        PartialEqAny::always_false()
    }

    async fn buildfiles<'a>(&self, cell: CellName) -> buck2_error::Result<Arc<[FileNameBuf]>> {
        DiceFileComputations::buildfiles(&mut self.0.get(), cell).await
    }
}

fn extended_ignore_error<'a>(
    ctx: &'a mut DiceComputations<'_>,
    path: CellPathRef<'a>,
) -> BoxFuture<'a, Option<ReadDirError>> {
    async move {
        match path.parent() {
            Some(parent) => match DiceFileComputations::read_dir_ext(ctx, parent).await {
                Ok(v) => {
                    // the parent can be read fine, check if this path is ignored first (if it's ignored it won't appear in the read_dir results).
                    if let Ok(FileIgnoreResult::Ignored(reason)) =
                        DiceFileComputations::is_ignored(ctx, path).await
                    {
                        return Some(ReadDirError::DirectoryIsIgnored(path.to_owned(), reason));
                    }

                    match path.path().file_name() {
                        Some(file_name) if !v.contains(file_name) => {
                            return Some(ReadDirError::DirectoryDoesNotExist(path.to_owned()));
                        }
                        _ => {}
                    }

                    match DiceFileComputations::read_path_metadata(ctx, path).await {
                        Ok(RawPathMetadata::Directory) => {}
                        Ok(RawPathMetadata::Symlink { .. }) => {
                            // not sure how we should handle symlink here, if it's pointing to a dir is it potentially correct?
                        }
                        Err(_) => {
                            // we ignore this, we don't know what the error is and so we can't be sure that
                            // it's not missing important data that the original error would have.
                        }
                        Ok(RawPathMetadata::File(..)) => {
                            return Some(ReadDirError::NotADirectory(
                                path.to_owned(),
                                "file".to_owned(),
                            ));
                        }
                    }

                    None
                }
                Err(e) => Some(e),
            },
            None => None,
        }
    }
    .boxed()
}

/// out-of-line impl for DiceComputations::read_dir_ext so it doesn't add noise to the api
async fn read_dir_ext(
    ctx: &mut DiceComputations<'_>,
    path: CellPathRef<'_>,
) -> Result<ReadDirOutput, ReadDirError> {
    match DiceFileComputations::read_dir(ctx, path).await {
        Ok(v) => Ok(v),
        Err(e) => match extended_ignore_error(ctx, path).await {
            Some(e) => Err(e),
            None => Err(e.into()),
        },
    }
}
