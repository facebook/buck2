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

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_hash::BuckDashMap;
use buck2_hash::BuckMutSet;
use cmp_any::PartialEqAny;
use derivative::Derivative;
use derive_more::Display;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::DiceTransactionUpdater;
use dice::EqualityBehavior;
use dice::InvalidationSourcePriority;
use dice::Key;
use dice::OkPagableValueSerialize;
use dice::UserComputationData;
use dice::ValueSerialize;
use dupe::Dupe;
use dupe::ResultDupedErrExt;
use pagable::Pagable;
use pagable::pagable_typetag;

use crate::dice::cells::HasCellResolver;
use crate::dice::data::HasIoProvider;
use crate::file_ops::baseline::FileSystemBaseline;
use crate::file_ops::baseline::HasFileSystemBaseline;
use crate::file_ops::delegate::FileOpsDelegate;
use crate::file_ops::metadata::RawDirEntry;
use crate::file_ops::metadata::RawPathMetadata;

/// A `FileOpsDelegate` implementation that calls out to the `IoProvider` to read files.
///
/// This is used for everything except 1) tests, and 2) external cells.
#[derive(Clone, Dupe, Derivative, Allocative, Pagable)]
#[derivative(PartialEq)]
pub(super) struct IoFileOpsDelegate {
    pub(super) cells: CellResolver,
    pub(super) cell: CellName,
    /// Not consulted by reads, which see the files as they are. It takes part in the delegate's
    /// equality, so that nothing read under one baseline is reused under another.
    pub(super) baseline: FileSystemBaseline,
}

/// The `Io*` keys are the DICE nodes at which file system state enters the graph; file watchers
/// invalidate them through the `FileChangeTracker`.
///
/// Each of them depends on the [`FileSystemBaseline`] so that nothing read under one baseline is
/// reused under another.
#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative, Pagable)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct IoReadFileKey(CellPath);

#[async_trait]
impl Key for IoReadFileKey {
    type Value = buck2_error::Result<()>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.get_file_system_baseline().await?;
        // FIXME(JakobDegen): This returning nothing is basically fine for now, it'd be good to get
        // that cleaned up a little bit though.
        Ok(())
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::AlwaysUnequal
    }

    fn invalidation_source_priority() -> InvalidationSourcePriority {
        InvalidationSourcePriority::High
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        OkPagableValueSerialize::<Self::Value>::new()
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative, Pagable)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct IoReadDirKey(CellPath);

#[async_trait]
impl Key for IoReadDirKey {
    type Value = buck2_error::Result<Arc<[RawDirEntry]>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.get_file_system_baseline().await?;
        io_read_dir_key_impl(ctx, self.0).await
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        })
    }

    fn validity(x: &Self::Value) -> bool {
        x.is_ok()
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        OkPagableValueSerialize::<Self::Value>::new()
    }
}

async fn io_read_dir_key_impl(
    ctx: &mut DiceComputations<'_>,
    path: CellPath,
) -> buck2_error::Result<Arc<[RawDirEntry]>> {
    let project_path = ctx.get_cell_resolver().await?.resolve_path(path)?;
    let read_dir_cache = ctx
        .per_transaction_data()
        .data
        .get::<ReadDirCache>()
        .expect("ReadDirCache is expected to be set.");
    if let Some(cached) = read_dir_cache.0.get(&project_path) {
        return Ok(cached.clone());
    };
    let mut entries = ctx
        .global_data()
        .get_io_provider()
        .read_dir(project_path.clone())
        .await
        .with_buck_error_context(|| format!("Error listing dir `{}`", project_path))?
        .into_entries();

    // Make sure entries are deterministic, since read_dir isn't.
    entries.sort_by(|a, b| a.file_name.cmp(&b.file_name));
    let entries: Arc<[RawDirEntry]> = Arc::from(entries);
    read_dir_cache.0.insert(project_path, entries.clone());

    Ok(entries)
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative, Pagable)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct IoPathMetadataKey(CellPath);

#[async_trait]
impl Key for IoPathMetadataKey {
    type Value = buck2_error::Result<Option<RawPathMetadata>>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.get_file_system_baseline().await?;
        let project_path = ctx.get_cell_resolver().await?.resolve_path(self.0)?;

        let Some(meta) = ctx
            .global_data()
            .get_io_provider()
            .read_path_metadata_if_exists(project_path.clone())
            .await
            .with_buck_error_context(|| {
                format!("Error accessing metadata for path `{project_path}`")
            })?
        else {
            return Ok(None);
        };
        let resolver = ctx.get_cell_resolver().await?;
        let meta = meta.map(|path| resolver.get_cell_path(&path));

        match meta {
            RawPathMetadata::Symlink { at, to: _ } => {
                ctx.compute(&IoReadFileKey(at))
                    .await?
                    .as_ref()
                    .duped_err()?;
            }
            _ => (),
        };

        Ok(Some(meta))
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        })
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

#[derive(Clone, Display, Allocative, Debug, Eq, Hash, PartialEq, Pagable)]
#[pagable_typetag(dice::DiceKeyDyn)]
#[display("{}", _0)]
struct IoExistsMatchingExactCaseKey(CellPath);

#[async_trait]
impl Key for IoExistsMatchingExactCaseKey {
    type Value = buck2_error::Result<bool>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.get_file_system_baseline().await?;
        let Some(dir) = self.0.parent() else {
            // FIXME(JakobDegen): Blindly assuming that cell roots exist isn't quite right, I'll fix
            // this later in the stack
            return Ok(true);
        };
        // FIXME(JakobDegen): Unwrap is ok because a parent exists, but there should be a better API
        // for this
        let entry = self.0.path().file_name().unwrap();
        let dir = io_read_dir_key_impl(ctx, dir).await?;
        Ok(dir.iter().any(|f| &*f.file_name == entry))
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        })
    }

    fn validity(x: &Self::Value) -> bool {
        x.is_ok()
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        OkPagableValueSerialize::<Self::Value>::new()
    }
}

#[pagable_typetag]
#[async_trait]
impl FileOpsDelegate for IoFileOpsDelegate {
    async fn read_file_if_exists(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<String>> {
        let path = CellPath::new(self.cell, path);
        ctx.compute(&IoReadFileKey(path))
            .await?
            .as_ref()
            .duped_err()?;
        let project_path = self.cells.resolve_path(path)?;
        let io = ctx.global_data().get_io_provider();
        io.read_file_if_exists(project_path).await
    }

    async fn read_dir(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Arc<[RawDirEntry]>> {
        let path = CellPath::new(self.cell, path);
        ctx.compute(&IoReadDirKey(path)).await?.dupe()
    }

    async fn read_path_metadata_if_exists(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        let path = CellPath::new(self.cell, path);
        ctx.compute(&IoPathMetadataKey(path)).await?.dupe()
    }

    async fn exists_matching_exact_case(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<bool> {
        let path = CellPath::new(self.cell, path);
        ctx.compute(&IoExistsMatchingExactCaseKey(path))
            .await?
            .dupe()
    }

    fn eq_token(&self) -> PartialEqAny<'_> {
        PartialEqAny::new(self)
    }
}

struct ReadDirCache(BuckDashMap<ProjectRelativePathBuf, Arc<[RawDirEntry]>>);

pub fn initialize_read_dir_cache(data: &mut UserComputationData) {
    data.data.set(ReadDirCache(Default::default()));
}

#[derive(Allocative)]
pub struct FileChangeTracker {
    files_to_dirty: BuckMutSet<IoReadFileKey>,
    dirs_to_dirty: BuckMutSet<IoReadDirKey>,
    paths_to_dirty: BuckMutSet<IoPathMetadataKey>,
    exists_matching_exact_case_to_dirty: BuckMutSet<IoExistsMatchingExactCaseKey>,

    maybe_modified_dirs: BuckMutSet<CellPath>,
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
                    self.entry_added_or_removed(p.0);
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
        self.paths_to_dirty.insert(IoPathMetadataKey(path));
        self.exists_matching_exact_case_to_dirty
            .insert(IoExistsMatchingExactCaseKey(path));
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
        self.dirs_to_dirty.insert(IoReadDirKey(path));
    }

    pub fn file_added_or_removed(&mut self, path: CellPath) {
        self.file_contents_changed(path);
        self.entry_added_or_removed(path);
    }

    pub fn dir_added_or_removed(&mut self, path: CellPath) {
        self.entry_added_or_removed(path);
    }

    pub fn file_contents_changed(&mut self, path: CellPath) {
        self.files_to_dirty.insert(IoReadFileKey(path));
        self.paths_to_dirty.insert(IoPathMetadataKey(path));
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
