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
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_fs::paths::file_name::FileNameBuf;
use buck2_hash::StdBuckHashSet;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceTransactionUpdater;
use dice::InvalidationSourcePriority;
use dice::Key;
use dice::OkPagableValueSerialize;
use dice::TodoValueSerialize;
use dice::ValueSerialize;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::future::BoxFuture;
use pagable::Pagable;
use pagable::pagable_typetag;

use crate::buildfiles::HasBuildfiles;
use crate::file_ops::delegate::get_delegated_file_ops;
use crate::file_ops::error::FileReadError;
use crate::file_ops::error::extended_ignore_error;
use crate::file_ops::metadata::RawPathMetadata;
use crate::file_ops::metadata::RawSymlink;
use crate::file_ops::metadata::ReadDirOutput;
use crate::ignores::file_ignores::FileIgnoreResult;
use crate::io::ReadDirError;

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

    /// Returns if a directory or file exists at the given path, but checks for an exact,
    /// case-sensitive match.
    ///
    /// Note that case-sensitive match is only done on the last element of the path, not any of the
    /// elements before.
    pub async fn exists_matching_exact_case(
        ctx: &mut DiceComputations<'_>,
        path: CellPathRef<'_>,
    ) -> buck2_error::Result<bool> {
        ctx.compute(&ExistsMatchingExactCaseKey(path.to_owned()))
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
        (ctx.compute(&ReadFileKey(Arc::new(path.to_owned())))
            .await??
            .0)()
        .await
    }

    /// Does not check if the path is ignored
    pub async fn read_file(
        ctx: &mut DiceComputations<'_>,
        path: CellPathRef<'_>,
    ) -> Result<String, FileReadError> {
        match Self::read_file_if_exists(ctx, path).await {
            Ok(result) => result.ok_or_else(|| FileReadError::NotFound(path.to_string())),
            Err(e) => Err(FileReadError::Buck(e)),
        }
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
    ) -> Result<RawPathMetadata, FileReadError> {
        match Self::read_path_metadata_if_exists(ctx, path).await {
            Ok(result) => result.ok_or_else(|| FileReadError::NotFound(path.to_string())),
            Err(e) => Err(FileReadError::Buck(e)),
        }
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

    pub async fn buildfiles(
        ctx: &mut DiceComputations<'_>,
        cell: CellName,
    ) -> buck2_error::Result<Arc<[FileNameBuf]>> {
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

#[derive(Allocative)]
pub struct FileChangeTracker {
    files_to_dirty: StdBuckHashSet<ReadFileKey>,
    dirs_to_dirty: StdBuckHashSet<ReadDirKey>,
    paths_to_dirty: StdBuckHashSet<PathMetadataKey>,
    exists_matching_exact_case_to_dirty: StdBuckHashSet<ExistsMatchingExactCaseKey>,

    maybe_modified_dirs: StdBuckHashSet<CellPath>,
}

impl FileChangeTracker {
    pub fn new() -> Self {
        Self {
            files_to_dirty: Default::default(),
            dirs_to_dirty: Default::default(),
            paths_to_dirty: Default::default(),
            maybe_modified_dirs: Default::default(),
            exists_matching_exact_case_to_dirty: Default::default(),
        }
    }

    pub fn write_to_dice(mut self, ctx: &mut DiceTransactionUpdater) -> buck2_error::Result<()> {
        // See comment on `dir_entries_changed_for_watchman_bug`
        for p in self.paths_to_dirty.clone() {
            if let Some(dir) = p.0.parent() {
                if self.maybe_modified_dirs.contains(&dir.to_owned()) {
                    self.entry_added_or_removed(p.0.clone());
                }
            }
        }

        ctx.changed(self.files_to_dirty)?;
        ctx.changed(self.dirs_to_dirty)?;
        ctx.changed(self.paths_to_dirty)?;
        ctx.changed(self.exists_matching_exact_case_to_dirty)?;

        Ok(())
    }

    fn entry_added_or_removed(&mut self, path: CellPath) {
        self.paths_to_dirty.insert(PathMetadataKey(path.clone()));
        self.exists_matching_exact_case_to_dirty
            .insert(ExistsMatchingExactCaseKey(path.clone()));
        let parent = path.parent();
        if let Some(parent) = parent {
            // The above can be None (validly!) if we have a cell we either create or delete.
            // That never happens in established repos, but if you are setting one up, it's not uncommon.
            // Since we don't include paths in different cells, the fact we don't dirty the parent
            // (which is in an enclosing cell) doesn't matter.
            self.insert_dir_keys(parent.to_owned());
        }
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
        self.file_contents_changed(path.clone());
        self.entry_added_or_removed(path);
    }

    pub fn dir_added_or_removed(&mut self, path: CellPath) {
        self.entry_added_or_removed(path);
    }

    pub fn file_contents_changed(&mut self, path: CellPath) {
        self.files_to_dirty
            .insert(ReadFileKey(Arc::new(path.clone())));
        self.paths_to_dirty.insert(PathMetadataKey(path.clone()));
    }

    /// Normally, buck does not need the file watcher to tell it that a directory's entries have
    /// changed. However, in some cases file watcher want to force-invalidate directory listings,
    /// and so this exists. It should not normally be used.
    pub fn dir_entries_changed_force_invalidate(&mut self, path: CellPath) {
        self.insert_dir_keys(path);
    }

    /// Normally, we ignore directory modification events from file watchers and instead compute
    /// them ourselves when a file in the directory is reported as having been added or removed.
    /// However, watchman has a bug in which it sometimes incorrectly doesn't report files as having
    /// been added/removed. We work around this by implementing some logic that marks a directory
    /// listing as being invalid if both the directory and at least one of its entries is reported
    /// as having been modified.
    ///
    /// We cannot unconditionally respect directory modification events from the file watcher, as it
    /// is not aware of our ignore rules.
    pub fn dir_entries_changed_for_watchman_bug(&mut self, path: CellPath) {
        self.maybe_modified_dirs.insert(path);
    }
}

/// The return value of a `ReadFileKey` computation.
///
/// Instead of the actual file contents, this is a closure that reads the actual file contents from
/// disk when invoked. This is done to ensure that we don't store the file contents in memory.
// FIXME(JakobDegen): `ReadFileKey` is not marked as transient if this returns an error, which is
// unfortunate.
#[derive(Clone, Dupe, Allocative)]
pub struct ReadFileProxy(
    #[allocative(skip)]
    Arc<dyn Fn() -> BoxFuture<'static, buck2_error::Result<Option<String>>> + Send + Sync>,
);

impl ReadFileProxy {
    /// This is a convenience method that avoids a little bit of boilerplate around boxing, and
    /// cloning the captures
    pub fn new_with_captures<D, F>(data: D, c: impl Fn(D) -> F + Send + Sync + 'static) -> Self
    where
        D: Clone + Send + Sync + 'static,
        F: Future<Output = buck2_error::Result<Option<String>>> + Send + 'static,
    {
        use futures::FutureExt;

        Self(Arc::new(move || {
            let data = data.clone();
            c(data).boxed()
        }))
    }
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative, Pagable)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct ReadFileKey(Arc<CellPath>);

#[async_trait]
impl Key for ReadFileKey {
    type Value = buck2_error::Result<ReadFileProxy>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let file_ops = get_delegated_file_ops(ctx, self.0.cell(), CheckIgnores::No).await?;

        // Check if the path goes through a symlink. We call the delegate
        // directly (not via PathMetadataKey) to avoid a circular DICE
        // dependency, since PathMetadataKey::compute depends on ReadFileKey.
        let metadata = file_ops
            .read_path_metadata_if_exists(ctx, self.0.path())
            .await?;
        if let Some(RawPathMetadata::Symlink {
            ref at,
            to: RawSymlink::Relative(ref target, _),
        }) = metadata
        {
            let at = at.as_ref();
            // If the symlink is an ancestor directory (at != requested path),
            // track it via PathMetadataKey. The file watcher will dirty
            // PathMetadataKey(at) when the symlink is retargeted, cascading
            // here.
            //
            // For leaf symlinks (at == self.0), the file watcher already
            // dirties ReadFileKey(self.0) directly, so no extra dep needed —
            // and adding one would cycle through PathMetadataKey.
            if at != self.0.as_ref() {
                ctx.compute(&PathMetadataKey(at.to_owned())).await??;
            }
            // Follow the symlink to the real path. Chains are handled
            // recursively since the target's compute also checks.
            return ctx.compute(&ReadFileKey(target.dupe())).await?;
        }

        // RawSymlink::External falls straight through to here
        file_ops.read_file_if_exists(ctx, self.0.path()).await
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        false
    }

    fn invalidation_source_priority() -> InvalidationSourcePriority {
        InvalidationSourcePriority::High
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        TodoValueSerialize::<Self::Value>::new()
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
        file_ops.read_dir(ctx, self.path.as_ref().path()).await
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
            .exists_matching_exact_case(self.0.path(), ctx)
            .await
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
            .read_path_metadata_if_exists(ctx, self.0.as_ref().path())
            .await?;

        if let Some(RawPathMetadata::Symlink {
            at: ref path,
            to: _,
        }) = res
        {
            ctx.compute(&ReadFileKey(path.dupe())).await??;
        }

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

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        OkPagableValueSerialize::<Self::Value>::new()
    }
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

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::cells::name::CellName;
    use dice::UserComputationData;
    use dice::testing::DiceBuilder;

    use super::*;
    use crate::file_ops::testing::TestFileOps;

    fn cell() -> CellName {
        CellName::testing_new("cell")
    }

    fn cell_path(p: &str) -> CellPath {
        CellPath::testing_new(&format!("cell//{p}"))
    }

    async fn build_dice(file_ops: &TestFileOps) -> dice::DiceTransaction {
        file_ops
            .mock_in_cell(cell(), DiceBuilder::new())
            .build(UserComputationData::new())
            .unwrap()
            .commit()
            .await
    }

    #[tokio::test]
    async fn read_file_through_leaf_symlink() {
        let file_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("target.txt"), "hello".to_owned())]),
            vec![(cell_path("link.txt"), cell_path("target.txt"))],
        );
        let mut ctx = build_dice(&file_ops).await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("link.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("hello"));
    }

    #[tokio::test]
    async fn read_file_through_symlink_to_nonexistent_then_retarget() {
        let file_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::new(),
            vec![(cell_path("link.txt"), cell_path("gone.txt"))],
        );
        let mut ctx = build_dice(&file_ops).await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("link.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content, None);

        // Retarget link.txt to a file that exists.
        // In real life, that's `ln -sf real.txt link.txt`.
        file_ops.replace_live(&TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("real.txt"), "hello".to_owned())]),
            vec![(cell_path("link.txt"), cell_path("real.txt"))],
        ));

        let mut ctx = dirty_and_commit(ctx, |t| {
            t.file_contents_changed(cell_path("link.txt"));
        })
        .await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("link.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("hello"));
    }

    #[tokio::test]
    async fn read_file_through_directory_symlink() {
        // a -> b (directory symlink), read a/e.txt which resolves to b/e.txt
        let file_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("b/e.txt"), "world".to_owned())]),
            vec![(cell_path("a"), cell_path("b"))],
        );
        let mut ctx = build_dice(&file_ops).await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("world"));
    }

    /// Tests chained relative directory symlinks:
    ///   a -> b, b/c -> ../d
    /// Reading a/c/e.txt resolves:
    ///   a/c/e.txt → (a is symlink) → b/c/e.txt → (b/c is symlink to ../d = d) → d/e.txt
    ///
    /// DICE dep graph:
    ///   ReadFileKey(a/c/e.txt)
    ///     ├─ PathMetadataKey(a)        ← tracks the a symlink
    ///     └─ ReadFileKey(b/c/e.txt)
    ///          ├─ PathMetadataKey(b/c)  ← tracks the b/c symlink
    ///          └─ ReadFileKey(d/e.txt)  ← the real file
    #[tokio::test]
    async fn read_file_through_chained_directory_symlinks() {
        let file_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("d/e.txt"), "chained".to_owned())]),
            vec![
                (cell_path("a"), cell_path("b")),
                (cell_path("b/c"), cell_path("d")),
            ],
        );
        let mut ctx = build_dice(&file_ops).await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/c/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("chained"));
    }

    /// Helper: dirty specific keys via FileChangeTracker and commit a new transaction.
    async fn dirty_and_commit(
        ctx: dice::DiceTransaction,
        dirty: impl FnOnce(&mut FileChangeTracker),
    ) -> dice::DiceTransaction {
        let mut updater = ctx.into_updater();
        let mut tracker = FileChangeTracker::new();
        dirty(&mut tracker);
        tracker.write_to_dice(&mut updater).unwrap();
        updater.commit().await
    }

    #[tokio::test]
    async fn dirty_target_invalidates_leaf_symlink() {
        let file_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("target.txt"), "v1".to_owned())]),
            vec![(cell_path("link.txt"), cell_path("target.txt"))],
        );
        let mut ctx = build_dice(&file_ops).await;

        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("link.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("v1"));

        file_ops.set_file_content(&cell_path("target.txt"), "v2");

        // No-op transaction: nothing dirtied → cached proxy returns old content.
        let mut ctx = dirty_and_commit(ctx, |_| {}).await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("link.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("v1"), "should still be cached");

        // File watcher: target.txt changed → recomputes, picks up new content.
        let mut ctx = dirty_and_commit(ctx, |t| {
            t.file_contents_changed(cell_path("target.txt"));
        })
        .await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("link.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(
            content.as_deref(),
            Some("v2"),
            "should see new content after dirtying target"
        );
    }

    /// Retarget a leaf symlink: link.txt changes from target.txt to other.txt.
    /// The file watcher dirties ReadFileKey(link.txt) directly (not via
    /// PathMetadataKey), exercising the at == self.0 branch in ReadFileKey::compute.
    #[tokio::test]
    async fn retarget_leaf_symlink_invalidates_read() {
        let file_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("target.txt"), "v1".to_owned())]),
            vec![(cell_path("link.txt"), cell_path("target.txt"))],
        );
        let mut ctx = build_dice(&file_ops).await;

        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("link.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("v1"));

        // Retarget link.txt -> other.txt
        file_ops.replace_live(&TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("other.txt"), "v2".to_owned())]),
            vec![(cell_path("link.txt"), cell_path("other.txt"))],
        ));

        // File watcher reports link.txt changed → dirties ReadFileKey(link.txt)
        let mut ctx = dirty_and_commit(ctx, |t| {
            t.file_contents_changed(cell_path("link.txt"));
        })
        .await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("link.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(
            content.as_deref(),
            Some("v2"),
            "should see new content after retargeting leaf symlink"
        );
    }

    #[tokio::test]
    async fn dirty_real_path_invalidates_through_directory_symlink() {
        // a -> b, reading a/e.txt (really b/e.txt)
        let file_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("b/e.txt"), "v1".to_owned())]),
            vec![(cell_path("a"), cell_path("b"))],
        );
        let mut ctx = build_dice(&file_ops).await;

        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("v1"));

        file_ops.set_file_content(&cell_path("b/e.txt"), "v2");

        // No-op: nothing dirty → cached
        let mut ctx = dirty_and_commit(ctx, |_| {}).await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("v1"), "should still be cached");

        // Dirty the real path b/e.txt → recomputes a/e.txt with new content
        let mut ctx = dirty_and_commit(ctx, |t| {
            t.file_contents_changed(cell_path("b/e.txt"));
        })
        .await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(
            content.as_deref(),
            Some("v2"),
            "should see new content after dirtying real path b/e.txt"
        );
    }

    #[tokio::test]
    async fn retarget_ancestor_symlink_invalidates_read() {
        // a -> b, reading a/e.txt (resolves to b/e.txt)
        let file_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("b/e.txt"), "v1".to_owned())]),
            vec![(cell_path("a"), cell_path("b"))],
        );
        let mut ctx = build_dice(&file_ops).await;

        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("v1"));

        // Retarget a -> c, with c/e.txt = "v2"
        file_ops.replace_live(&TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("c/e.txt"), "v2".to_owned())]),
            vec![(cell_path("a"), cell_path("c"))],
        ));

        // File watcher reports `a` symlink retargeted →
        // PathMetadataKey(a) dirty → ReadFileKey(a/e.txt) recomputes →
        // now resolves to c/e.txt
        let mut ctx = dirty_and_commit(ctx, |t| {
            t.file_contents_changed(cell_path("a"));
        })
        .await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(
            content.as_deref(),
            Some("v2"),
            "should see new content after retargeting ancestor symlink"
        );
    }

    /// a -> b, b/c -> ../d, reading a/c/e.txt (resolves to d/e.txt)
    /// Dirty the real file d/e.txt and verify it cascades through both hops.
    #[tokio::test]
    async fn dirty_real_path_invalidates_through_chained_symlinks() {
        let file_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("d/e.txt"), "v1".to_owned())]),
            vec![
                (cell_path("a"), cell_path("b")),
                (cell_path("b/c"), cell_path("d")),
            ],
        );
        let mut ctx = build_dice(&file_ops).await;

        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/c/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("v1"));

        file_ops.set_file_content(&cell_path("d/e.txt"), "v2");

        // No-op → cached
        let mut ctx = dirty_and_commit(ctx, |_| {}).await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/c/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("v1"), "should still be cached");

        // Dirty d/e.txt → cascades through b/c/e.txt → a/c/e.txt
        let mut ctx = dirty_and_commit(ctx, |t| {
            t.file_contents_changed(cell_path("d/e.txt"));
        })
        .await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/c/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(
            content.as_deref(),
            Some("v2"),
            "should see new content after dirtying real path d/e.txt"
        );
    }

    /// a -> b, b/c -> ../d, reading a/c/e.txt (resolves to d/e.txt)
    /// Retarget b/c from ../d to ../f, with f/e.txt = "v2".
    #[tokio::test]
    async fn retarget_intermediate_symlink_invalidates_through_chain() {
        let file_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("d/e.txt"), "v1".to_owned())]),
            vec![
                (cell_path("a"), cell_path("b")),
                (cell_path("b/c"), cell_path("d")),
            ],
        );
        let mut ctx = build_dice(&file_ops).await;

        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/c/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("v1"));

        // Retarget b/c -> ../f, with f/e.txt = "v2"
        file_ops.replace_live(&TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("f/e.txt"), "v2".to_owned())]),
            vec![
                (cell_path("a"), cell_path("b")),
                (cell_path("b/c"), cell_path("f")),
            ],
        ));

        // Dirty b/c → cascades through a/c/e.txt
        let mut ctx = dirty_and_commit(ctx, |t| {
            t.file_contents_changed(cell_path("b/c"));
        })
        .await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/c/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(
            content.as_deref(),
            Some("v2"),
            "should see new content after retargeting intermediate symlink b/c"
        );
    }

    // ---- Cross-cell helpers ----

    fn other_cell() -> CellName {
        CellName::testing_new("other")
    }

    fn other_cell_path(p: &str) -> CellPath {
        CellPath::testing_new(&format!("other//{p}"))
    }

    async fn build_dice_two_cells(
        cell_ops: &TestFileOps,
        other_ops: &TestFileOps,
    ) -> dice::DiceTransaction {
        let builder = cell_ops.mock_in_cell(cell(), DiceBuilder::new());
        let builder = other_ops.mock_in_cell(other_cell(), builder);
        builder
            .build(UserComputationData::new())
            .unwrap()
            .commit()
            .await
    }

    /// Leaf symlink in one cell pointing to a file in another cell.
    /// cell//link.txt → other//target.txt
    #[tokio::test]
    async fn read_file_through_cross_cell_leaf_symlink() {
        let cell_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::new(),
            vec![(cell_path("link.txt"), other_cell_path("target.txt"))],
        );
        let other_ops = TestFileOps::new_with_files(BTreeMap::from([(
            other_cell_path("target.txt"),
            "cross-cell".to_owned(),
        )]));
        let mut ctx = build_dice_two_cells(&cell_ops, &other_ops).await;

        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("link.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("cross-cell"));
    }

    /// Dirty the target file in another cell; verify invalidation cascades
    /// back through the cross-cell symlink.
    #[tokio::test]
    async fn dirty_target_invalidates_cross_cell_leaf_symlink() {
        let cell_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::new(),
            vec![(cell_path("link.txt"), other_cell_path("target.txt"))],
        );
        let other_ops = TestFileOps::new_with_files(BTreeMap::from([(
            other_cell_path("target.txt"),
            "v1".to_owned(),
        )]));
        let mut ctx = build_dice_two_cells(&cell_ops, &other_ops).await;

        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("link.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("v1"));

        other_ops.set_file_content(&other_cell_path("target.txt"), "v2");

        let mut ctx = dirty_and_commit(ctx, |t| {
            t.file_contents_changed(other_cell_path("target.txt"));
        })
        .await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("link.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(
            content.as_deref(),
            Some("v2"),
            "should see new content after dirtying cross-cell target"
        );
    }

    /// Directory symlink crossing cells: cell//a → other//b, read cell//a/e.txt.
    #[tokio::test]
    async fn read_file_through_cross_cell_directory_symlink() {
        let cell_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::new(),
            vec![(cell_path("a"), other_cell_path("b"))],
        );
        let other_ops = TestFileOps::new_with_files(BTreeMap::from([(
            other_cell_path("b/e.txt"),
            "remote-dir".to_owned(),
        )]));
        let mut ctx = build_dice_two_cells(&cell_ops, &other_ops).await;

        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("remote-dir"));
    }

    /// External symlinks (pointing outside the project root) are not
    /// DICE-tracked. ReadFileKey falls through to the delegate, which
    /// reads the content directly.
    ///
    /// We will then get stale reads.
    #[tokio::test]
    async fn external_symlink_falls_through_to_delegate() {
        use std::path::PathBuf;

        use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;

        use crate::external_symlink::ExternalSymlink;

        let symlink = Arc::new(
            ExternalSymlink::new(
                PathBuf::from("/outside/project"),
                ForwardRelativePathBuf::empty(),
            )
            .unwrap(),
        );
        let file_ops = TestFileOps::new_with_symlinks_and_external_content(BTreeMap::from([(
            cell_path("ext_link"),
            (symlink, "external-content".to_owned()),
        )]));
        let mut ctx = build_dice(&file_ops).await;

        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("ext_link").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("external-content"));

        // The external target changes, but the file watcher has no visibility
        // into paths outside the project — nothing gets dirtied.
        file_ops.set_external_symlink_content(&cell_path("ext_link"), "updated-external");

        let mut ctx = dirty_and_commit(ctx, |_| {}).await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("ext_link").as_ref())
                .await
                .unwrap();
        assert_eq!(
            content.as_deref(),
            Some("external-content"),
            "stale: DICE has no dep on the external target, so the cached proxy returns old content"
        );
    }

    /// a -> b, b/c -> ../d, reading a/c/e.txt
    /// Retarget a from b to g (where g/c -> ../h, h/e.txt = "v2").
    #[tokio::test]
    async fn retarget_outermost_symlink_invalidates_through_chain() {
        let file_ops = TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("d/e.txt"), "v1".to_owned())]),
            vec![
                (cell_path("a"), cell_path("b")),
                (cell_path("b/c"), cell_path("d")),
            ],
        );
        let mut ctx = build_dice(&file_ops).await;

        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/c/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(content.as_deref(), Some("v1"));

        // Retarget a -> g, with g/c -> ../h, h/e.txt = "v2"
        file_ops.replace_live(&TestFileOps::new_with_files_and_symlinks(
            BTreeMap::from([(cell_path("h/e.txt"), "v2".to_owned())]),
            vec![
                (cell_path("a"), cell_path("g")),
                (cell_path("g/c"), cell_path("h")),
            ],
        ));

        // Dirty `a` → ReadFileKey(a/c/e.txt) recomputes entire chain
        let mut ctx = dirty_and_commit(ctx, |t| {
            t.file_contents_changed(cell_path("a"));
        })
        .await;
        let content =
            DiceFileComputations::read_file_if_exists(&mut ctx, cell_path("a/c/e.txt").as_ref())
                .await
                .unwrap();
        assert_eq!(
            content.as_deref(),
            Some("v2"),
            "should see new content after retargeting outermost symlink a"
        );
    }
}
