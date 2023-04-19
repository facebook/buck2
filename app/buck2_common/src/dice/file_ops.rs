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
use anyhow::Context;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_events::dispatch::console_message;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceTransactionUpdater;
use dice::Key;
use dupe::Dupe;
use gazebo::cmp::PartialEqAny;
use more_futures::cancellation::CancellationContext;

use crate::dice::cells::HasCellResolver;
use crate::dice::data::HasIoProvider;
use crate::dice::file_ops::keys::FileOpsKey;
use crate::dice::file_ops::keys::FileOpsValue;
use crate::file_ops::FileOps;
use crate::file_ops::RawDirEntry;
use crate::file_ops::RawPathMetadata;
use crate::file_ops::ReadDirOutput;
use crate::file_ops::SimpleDirEntry;
use crate::ignores::AllCellIgnores;
use crate::ignores::HasAllCellIgnores;
use crate::ignores::MaybeIgnoredCellRelativePath;
use crate::io::IoProvider;
use crate::result::SharedResult;
use crate::result::ToSharedResultExt;
use crate::result::ToUnsharedResultExt;

pub trait HasFileOps<'c> {
    type T: FileOps;
    fn file_ops(&'c self) -> Self::T;
}

impl<'c> HasFileOps<'c> for DiceComputations {
    type T = DiceFileOps<'c>;
    fn file_ops(&'c self) -> DiceFileOps<'c> {
        DiceFileOps(self)
    }
}

// TODO(cjhopman, bobyf): This FileToken can go away once Dice has support for
// transient values.
/// This is used as the "result" of a read_file computation so that we don't
/// need to store the file content's in dice's cache.
#[derive(Clone, Dupe, Allocative)]
struct FileToken(Arc<CellPath>);

impl FileToken {
    async fn read_if_exists(&self, fs: &dyn FileOps) -> anyhow::Result<Option<String>> {
        fs.read_file_if_exists((*self.0).as_ref()).await
    }
}

#[derive(Clone, Dupe, Allocative)]
pub struct DiceFileOps<'c>(#[allocative(skip)] pub &'c DiceComputations);

pub mod keys {
    use std::sync::Arc;

    use allocative::Allocative;
    use derive_more::Display;
    use dupe::Dupe;

    use crate::file_ops::FileOps;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    #[display(fmt = "{:?}", self)]
    pub struct FileOpsKey();

    #[derive(Dupe, Clone, Allocative)]
    pub struct FileOpsValue(#[allocative(skip)] pub Arc<dyn FileOps>);
}

async fn get_default_file_ops(dice: &DiceComputations) -> SharedResult<Arc<dyn FileOps>> {
    #[derive(Clone, Dupe, Derivative, Allocative)]
    #[derivative(PartialEq)]
    struct DiceFileOpsDelegate {
        // Safe to ignore because `io` does not change during the lifetime of the daemon.
        #[derivative(PartialEq = "ignore")]
        io: Arc<dyn IoProvider>,
        cells: CellResolver,
        ignores: Arc<AllCellIgnores>,
    }

    impl DiceFileOpsDelegate {
        fn resolve(&self, path: CellPathRef) -> anyhow::Result<ProjectRelativePathBuf> {
            let cell_root = self.resolve_cell_root(path.cell())?;
            Ok(cell_root.project_relative_path().join(path.path()))
        }

        fn resolve_cell_root(&self, cell: CellName) -> anyhow::Result<CellRootPathBuf> {
            Ok(self.cells.get(cell).unwrap().path().to_buf())
        }

        fn get_cell_path(&self, path: &ProjectRelativePath) -> anyhow::Result<CellPath> {
            self.cells.get_cell_path(path)
        }

        fn io_provider(&self) -> &dyn IoProvider {
            self.io.as_ref()
        }
    }

    #[async_trait]
    impl FileOps for DiceFileOpsDelegate {
        async fn read_file_if_exists(
            &self,
            path: CellPathRef<'async_trait>,
        ) -> anyhow::Result<Option<String>> {
            // TODO(cjhopman): error on ignored paths, maybe.
            let project_path = self.resolve(path)?;
            self.io_provider().read_file_if_exists(project_path).await
        }

        async fn read_dir(&self, path: CellPathRef<'async_trait>) -> anyhow::Result<ReadDirOutput> {
            // TODO(cjhopman): This should also probably verify that the parent chain is not ignored.
            self.ignores
                .check_ignored(path.cell(), MaybeIgnoredCellRelativePath::new(path.path()))?
                .into_result()
                .with_context(|| format!("Error checking whether dir `{}` is ignored", path))?;

            let project_path = self.resolve(path)?;
            let mut entries = self
                .io_provider()
                .read_dir(project_path)
                .await
                .with_context(|| format!("Error listing dir `{}`", path))?;

            // Make sure entries are deterministic, since read_dir isn't.
            entries.sort_by(|a, b| a.file_name.cmp(&b.file_name));

            let is_ignored = |file_name: &str| {
                let mut cell_relative_path_buf;
                let cell_relative_path: &str = if path.path().is_empty() {
                    file_name
                } else {
                    cell_relative_path_buf =
                        String::with_capacity(path.path().as_str().len() + 1 + file_name.len());
                    cell_relative_path_buf.push_str(path.path().as_str());
                    cell_relative_path_buf.push('/');
                    cell_relative_path_buf.push_str(file_name);
                    &cell_relative_path_buf
                };

                let cell_relative_path =
                    MaybeIgnoredCellRelativePath::unchecked_new(cell_relative_path);
                let is_ignored = self
                    .ignores
                    .check_ignored(path.cell(), cell_relative_path)?
                    .is_ignored();
                anyhow::Ok(is_ignored)
            };

            // Filter out any entries that are ignored.
            let mut included_entries = Vec::new();
            for e in entries {
                let RawDirEntry {
                    file_type,
                    file_name,
                } = e;

                if !is_ignored(&file_name)? {
                    let file_name = match FileNameBuf::try_from_or_get_back(file_name) {
                        Ok(file_name) => file_name,
                        Err(file_name) => {
                            console_message(format!(
                                "File name `{file_name}` is not valid. \
                                    Add the path to `project.ignore` to mute this message",
                            ));
                            continue;
                        }
                    };
                    included_entries.push(SimpleDirEntry {
                        file_type,
                        file_name,
                    });
                }
            }

            Ok(ReadDirOutput {
                included: included_entries.into(),
            })
        }

        async fn read_path_metadata_if_exists(
            &self,
            path: CellPathRef<'async_trait>,
        ) -> anyhow::Result<Option<RawPathMetadata>> {
            let project_path = self.resolve(path)?;

            let res = self
                .io_provider()
                .read_path_metadata_if_exists(project_path)
                .await
                .with_context(|| format!("Error accessing metadata for path `{}`", path))?;
            res.map(|meta| meta.try_map(|path| Ok(Arc::new(self.get_cell_path(&path)?))))
                .transpose()
        }

        async fn is_ignored(&self, path: CellPathRef<'async_trait>) -> anyhow::Result<bool> {
            Ok(self
                .ignores
                .check_ignored(path.cell(), MaybeIgnoredCellRelativePath::new(path.path()))?
                .is_ignored())
        }

        fn eq_token(&self) -> PartialEqAny {
            PartialEqAny::new(self)
        }
    }

    #[async_trait]
    impl Key for FileOpsKey {
        type Value = SharedResult<FileOpsValue>;
        async fn compute(
            &self,
            ctx: &DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            let cells = ctx.get_cell_resolver().await?;
            let io = ctx.global_data().get_io_provider();

            let ignores = ctx.new_all_cell_ignores().await?;

            Ok(FileOpsValue(Arc::new(DiceFileOpsDelegate {
                io,
                cells,
                ignores,
            })))
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            match (x, y) {
                (Ok(x), Ok(y)) => *x.0 == *y.0,
                _ => false,
            }
        }

        fn validity(x: &Self::Value) -> bool {
            x.is_ok()
        }
    }

    Ok(dice.compute(&FileOpsKey()).await??.0)
}

#[derive(Allocative)]
pub struct FileChangeTracker {
    files_to_dirty: HashSet<ReadFileKey>,
    dirs_to_dirty: HashSet<ReadDirKey>,
    paths_to_dirty: HashSet<PathMetadataKey>,
}

impl FileChangeTracker {
    pub fn new() -> Self {
        Self {
            files_to_dirty: Default::default(),
            dirs_to_dirty: Default::default(),
            paths_to_dirty: Default::default(),
        }
    }

    pub fn write_to_dice(self, ctx: &mut DiceTransactionUpdater) -> anyhow::Result<()> {
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

    pub fn file_added_or_removed(&mut self, path: CellPath) {
        let parent = path.parent();

        self.file_contents_modify(path.clone());
        if let Some(parent) = parent {
            // The above can be None (validly!) if we have a cell we either create or delete.
            // That never happens in established repos, but if you are setting one up, it's not uncommon.
            // Since we don't include paths in different cells, the fact we don't dirty the parent
            // (which is in an enclosing cell) doesn't matter.
            self.dirs_to_dirty.insert(ReadDirKey(parent.to_owned()));
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
            self.dirs_to_dirty
                .extend([ReadDirKey(path), ReadDirKey(parent)]);
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
        self.dirs_to_dirty.insert(ReadDirKey(path));
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
    type Value = FileToken;
    async fn compute(
        &self,
        _ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        FileToken(self.0.dupe())
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        false
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
struct ReadDirKey(CellPath);

#[async_trait]
impl Key for ReadDirKey {
    type Value = SharedResult<ReadDirOutput>;
    async fn compute(
        &self,
        ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        get_default_file_ops(ctx)
            .await?
            .read_dir(self.0.as_ref())
            .await
            .shared_error()
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
    type Value = SharedResult<Option<RawPathMetadata>>;
    async fn compute(
        &self,
        ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let res = get_default_file_ops(ctx)
            .await?
            .read_path_metadata_if_exists(self.0.as_ref())
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
}

#[async_trait]
impl<'c> FileOps for DiceFileOps<'c> {
    async fn read_file_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<Option<String>> {
        let path = path.to_owned();
        let file_ops = get_default_file_ops(self.0).await?;

        self.0
            .compute(&ReadFileKey(Arc::new(path)))
            .await?
            .read_if_exists(&*file_ops)
            .await
    }

    async fn read_dir(&self, path: CellPathRef<'async_trait>) -> anyhow::Result<ReadDirOutput> {
        self.0
            .compute(&ReadDirKey(path.to_owned()))
            .await?
            .unshared_error()
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<Option<RawPathMetadata>> {
        self.0
            .compute(&PathMetadataKey(path.to_owned()))
            .await?
            .unshared_error()
    }

    async fn is_ignored(&self, path: CellPathRef<'async_trait>) -> anyhow::Result<bool> {
        let file_ops = get_default_file_ops(self.0).await?;
        file_ops.is_ignored(path).await
    }

    fn eq_token(&self) -> PartialEqAny {
        // We do not store this on DICE, so we don't care about equality.
        // Also we cannot do `PartialEqAny` here because `Self` is not `'static`.
        PartialEqAny::always_false()
    }
}

pub mod testing {
    pub use super::keys::FileOpsKey;
}
