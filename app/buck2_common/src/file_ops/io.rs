/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use cmp_any::PartialEqAny;
use dashmap::DashMap;
use derivative::Derivative;
use derive_more::Display;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::DiceTransactionUpdater;
use dice::InvalidationSourcePriority;
use dice::Key;
use dice::UserComputationData;
use dupe::Dupe;

use crate::dice::cells::HasCellResolver;
use crate::dice::data::HasIoProvider;
use crate::file_ops::delegate::FileOpsDelegate;
use crate::file_ops::dice::ReadFileProxy;
use crate::file_ops::metadata::RawDirEntry;
use crate::file_ops::metadata::RawPathMetadata;

/// A `FileOpsDelegate` implementation that calls out to the `IoProvider` to read files.
///
/// This is used for everything except 1) tests, and 2) external cells.
#[derive(Clone, Dupe, Derivative, Allocative)]
#[derivative(PartialEq)]
pub(super) struct IoFileOpsDelegate {
    pub(super) cells: CellResolver,
    pub(super) cell: CellName,
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
struct IoReadFileKey(CellPath);

#[async_trait]
impl Key for IoReadFileKey {
    type Value = buck2_error::Result<ReadFileProxy>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let path = ctx
            .get_cell_resolver()
            .await?
            .resolve_path(self.0.as_ref())?;
        Ok(ReadFileProxy::new_with_captures(
            (path, ctx.global_data().get_io_provider()),
            |(project_path, io)| async move { io.read_file_if_exists(project_path).await },
        ))
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        false
    }

    fn invalidation_source_priority() -> InvalidationSourcePriority {
        InvalidationSourcePriority::High
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
struct IoReadDirKey(CellPath);

#[async_trait]
impl Key for IoReadDirKey {
    type Value = buck2_error::Result<Arc<[RawDirEntry]>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        io_read_dir_key_impl(ctx, self.0.as_ref()).await
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

async fn io_read_dir_key_impl(
    ctx: &mut DiceComputations<'_>,
    path: CellPathRef<'_>,
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
        .with_buck_error_context(|| format!("Error listing dir `{}`", project_path))?;

    // Make sure entries are deterministic, since read_dir isn't.
    entries.sort_by(|a, b| a.file_name.cmp(&b.file_name));
    let entries: Arc<[RawDirEntry]> = Arc::from(entries);
    read_dir_cache.0.insert(project_path, entries.clone());

    Ok(entries)
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
struct IoPathMetadataKey(CellPath);

#[async_trait]
impl Key for IoPathMetadataKey {
    type Value = buck2_error::Result<Option<RawPathMetadata>>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let project_path = ctx
            .get_cell_resolver()
            .await?
            .resolve_path(self.0.as_ref())?;

        let Some(meta) = ctx
            .global_data()
            .get_io_provider()
            .read_path_metadata_if_exists(project_path.clone())
            .await
            .with_buck_error_context(|| {
                format!("Error accessing metadata for path `{}`", project_path)
            })?
        else {
            return Ok(None);
        };
        let resolver = ctx.get_cell_resolver().await?;
        let meta =
            meta.try_map(|path| buck2_error::Ok(Arc::new(resolver.get_cell_path(&path)?)))?;

        match meta {
            RawPathMetadata::Symlink {
                at: ref path,
                to: _,
            } => {
                ctx.compute(&IoReadFileKey((**path).clone())).await??;
            }
            _ => (),
        };

        Ok(Some(meta))
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

#[derive(Clone, Display, Allocative, Debug, Eq, Hash, PartialEq)]
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

#[async_trait]
impl FileOpsDelegate for IoFileOpsDelegate {
    async fn read_file_if_exists(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<ReadFileProxy> {
        let path = CellPath::new(self.cell, path.to_buf());
        ctx.compute(&IoReadFileKey(path)).await?
    }

    async fn read_dir(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Arc<[RawDirEntry]>> {
        let path = CellPath::new(self.cell, path.to_buf());
        ctx.compute(&IoReadDirKey(path)).await?
    }

    async fn read_path_metadata_if_exists(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        let path = CellPath::new(self.cell, path.to_buf());
        ctx.compute(&IoPathMetadataKey(path)).await?
    }

    async fn exists_matching_exact_case(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<bool> {
        let path = CellPath::new(self.cell, path.to_buf());
        ctx.compute(&IoExistsMatchingExactCaseKey(path)).await?
    }

    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }
}

struct ReadDirCache(DashMap<ProjectRelativePathBuf, Arc<[RawDirEntry]>>);

pub fn initialize_read_dir_cache(data: &mut UserComputationData) {
    data.data.set(ReadDirCache(Default::default()));
}

#[derive(Allocative)]
pub struct FileChangeTracker {
    files_to_dirty: HashSet<IoReadFileKey>,
    dirs_to_dirty: HashSet<IoReadDirKey>,
    paths_to_dirty: HashSet<IoPathMetadataKey>,
    exists_matching_exact_case_to_dirty: HashSet<IoExistsMatchingExactCaseKey>,

    maybe_modified_dirs: HashSet<CellPath>,
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
        self.paths_to_dirty.insert(IoPathMetadataKey(path.clone()));
        self.exists_matching_exact_case_to_dirty
            .insert(IoExistsMatchingExactCaseKey(path.clone()));
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
        self.file_contents_changed(path.clone());
        self.entry_added_or_removed(path);
    }

    pub fn dir_added_or_removed(&mut self, path: CellPath) {
        self.entry_added_or_removed(path);
    }

    pub fn file_contents_changed(&mut self, path: CellPath) {
        self.files_to_dirty.insert(IoReadFileKey(path.clone()));
        self.paths_to_dirty.insert(IoPathMetadataKey(path.clone()));
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
