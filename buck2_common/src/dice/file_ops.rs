/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRelativePath;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceTransaction;
use dice::Key;
use gazebo::cmp::PartialEqAny;
use gazebo::prelude::*;
use itertools::Itertools;

use crate::dice::cells::HasCellResolver;
use crate::dice::data::HasIoProvider;
use crate::dice::file_ops::keys::FileOpsKey;
use crate::dice::file_ops::keys::FileOpsValue;
use crate::file_ops::DefaultFileOpsDelegate;
use crate::file_ops::FileIgnoreResult;
use crate::file_ops::FileIgnores;
use crate::file_ops::FileOps;
use crate::file_ops::RawPathMetadata;
use crate::file_ops::ReadDirOutput;
use crate::file_ops::SimpleDirEntry;
use crate::io::IoProvider;
use crate::legacy_configs::dice::HasLegacyConfigs;
use crate::result::SharedResult;

pub trait HasFileOps<'c> {
    type T: FileOps;
    fn file_ops(&'c self) -> Self::T;
}

pub trait FileChangeHandler {
    fn file_changed(&mut self, path: CellPath);
    fn file_removed(&mut self, path: CellPath);
    fn file_added(&mut self, path: CellPath);
    fn dir_changed(&mut self, path: CellPath);
    fn dir_added(&mut self, path: CellPath);
    fn dir_removed(&mut self, path: CellPath);
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
    async fn read(&self, fs: &dyn FileOps) -> anyhow::Result<String> {
        fs.read_file(&self.0).await
    }
}

#[derive(Clone, Dupe, Allocative)]
pub struct DiceFileOps<'c>(#[allocative(skip)] pub &'c DiceComputations);

pub mod keys {
    use std::sync::Arc;

    use allocative::Allocative;
    use derive_more::Display;
    use gazebo::prelude::*;

    use crate::file_ops::FileOps;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    #[display(fmt = "{:?}", self)]
    pub struct FileOpsKey();

    #[derive(Dupe, Clone, Allocative)]
    pub struct FileOpsValue(#[allocative(skip)] pub Arc<dyn FileOps>);
}

async fn get_default_file_ops(dice: &DiceComputations) -> SharedResult<Arc<dyn FileOps>> {
    #[derive(Clone, Dupe, PartialEq, Allocative)]
    struct DiceFileOpsDelegate {
        io: PartialEqWrapper<Arc<dyn IoProvider>>,
        cells: CellResolver,
        ignores: Arc<HashMap<CellName, FileIgnores>>,
    }

    // NOTE: We need this because derive(PartialEq) fails to compile otherwise on
    // DiceFileOpsDelegate: move occurs because `*__self_1_0` has type `std::sync::Arc<dyn
    // IoProvider>`, which does not implement the `Copy` trait.
    #[derive(Clone, Dupe, PartialEq, Allocative)]
    pub struct PartialEqWrapper<T>(T);

    impl<T> std::ops::Deref for PartialEqWrapper<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    #[async_trait]
    impl DefaultFileOpsDelegate for DiceFileOpsDelegate {
        fn check_ignored(&self, path: &CellPath) -> anyhow::Result<FileIgnoreResult> {
            Ok(self
                .ignores
                .get(path.cell())
                .unwrap_or_else(|| {
                    panic!(
                        "Should've had an ignore spec for `{}`. Had `{}`",
                        path.cell(),
                        self.ignores.keys().join(", ")
                    )
                })
                .check(path.path()))
        }

        fn resolve_cell_root(&self, cell: &CellName) -> anyhow::Result<CellRootPathBuf> {
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
    impl Key for FileOpsKey {
        type Value = SharedResult<FileOpsValue>;
        async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
            let cells = ctx.get_cell_resolver().await?;
            let configs = ctx.get_legacy_configs_on_dice().await?;
            let io = ctx.global_data().get_io_provider();

            let cell_paths: Vec<_> = cells.cells().map(|e| (e.1.name(), e.1.path())).collect();
            let mut ignores = HashMap::new();

            for (cell_name, instance) in cells.cells() {
                let this_path = instance.path();
                let config = configs.get(cell_name).unwrap();
                let ignore_spec = config.get("project", "ignore")?;
                let ignore_spec = ignore_spec.as_ref().map_or("", |s| &**s);

                let cell_ignores =
                    FileIgnores::new_for_interpreter(ignore_spec, &cell_paths, this_path)?;
                ignores.insert(cell_name.clone(), cell_ignores);
            }

            Ok(FileOpsValue(Arc::new(DiceFileOpsDelegate {
                io: PartialEqWrapper(io),
                cells,
                ignores: Arc::new(ignores),
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

fn panic_expected_parent(path: &CellPath) -> ! {
    panic!(
        "a file/dir in the repo must have a parent, but `{}` had none",
        path
    )
}

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

    pub fn write_to_dice(self, ctx: &DiceTransaction) -> anyhow::Result<()> {
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

    fn file_existence_modify(&mut self, path: CellPath) {
        let parent = path
            .parent()
            .unwrap_or_else(|| panic_expected_parent(&path));

        self.file_contents_modify(path);
        self.dirs_to_dirty.insert(ReadDirKey(parent));
    }

    fn dir_existence_modify(&mut self, path: CellPath) {
        self.paths_to_dirty.insert(PathMetadataKey(path.clone()));
        if let Some(parent) = path.parent() {
            // The above can be None (validly!) if we have a cell we either create or delete.
            // That never happens in established repos, but if you are setting one up, it's not uncommon.
            // Since we don't include paths in different cells, the fact we don't dirty the parent
            // (which is in an enclosing cell) doesn't matter.
            self.dirs_to_dirty
                .extend([ReadDirKey(path), ReadDirKey(parent)]);
        }
    }
}

impl FileChangeHandler for FileChangeTracker {
    fn file_changed(&mut self, path: CellPath) {
        self.file_contents_modify(path)
    }

    fn file_removed(&mut self, path: CellPath) {
        self.file_existence_modify(path)
    }

    fn file_added(&mut self, path: CellPath) {
        self.file_existence_modify(path)
    }

    fn dir_changed(&mut self, path: CellPath) {
        self.paths_to_dirty.insert(PathMetadataKey(path.clone()));
        self.dirs_to_dirty.insert(ReadDirKey(path));
    }

    fn dir_added(&mut self, path: CellPath) {
        self.dir_existence_modify(path)
    }

    fn dir_removed(&mut self, path: CellPath) {
        self.dir_existence_modify(path)
    }
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "{}", _0)]
struct ReadFileKey(Arc<CellPath>);

#[async_trait]
impl Key for ReadFileKey {
    type Value = FileToken;
    async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
        FileToken(self.0.dupe())
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        false
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "{}", _0)]
struct ReadDirKey(CellPath);

#[async_trait]
impl Key for ReadDirKey {
    type Value = SharedResult<ReadDirOutput>;
    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        get_default_file_ops(ctx)
            .await?
            .read_dir_with_ignores(&self.0)
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
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "{}", _0)]
struct PathMetadataKey(CellPath);

#[async_trait]
impl Key for PathMetadataKey {
    type Value = SharedResult<Option<RawPathMetadata>>;
    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        let res = get_default_file_ops(ctx)
            .await?
            .read_path_metadata_if_exists(&self.0)
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
    async fn read_file(&self, path: &CellPath) -> anyhow::Result<String> {
        let file_ops = get_default_file_ops(self.0).await?;

        self.0
            .compute(&ReadFileKey(Arc::new(path.to_owned())))
            .await?
            .read(&*file_ops)
            .await
    }

    async fn read_dir(&self, path: &CellPath) -> SharedResult<Arc<Vec<SimpleDirEntry>>> {
        Ok(self.read_dir_with_ignores(path).await?.included)
    }

    async fn read_dir_with_ignores(&self, path: &CellPath) -> SharedResult<ReadDirOutput> {
        self.0.compute(&ReadDirKey(path.clone())).await?
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: &CellPath,
    ) -> SharedResult<Option<RawPathMetadata>> {
        self.0.compute(&PathMetadataKey(path.clone())).await?
    }

    async fn is_ignored(&self, path: &CellPath) -> anyhow::Result<bool> {
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
