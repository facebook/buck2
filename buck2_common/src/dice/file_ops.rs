/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::CellName;
use buck2_core::cells::CellResolver;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use gazebo::cmp::PartialEqAny;
use gazebo::prelude::*;
use itertools::Itertools;

use crate::dice::cells::HasCellResolver;
use crate::dice::data::HasIoProvider;
use crate::dice::file_ops::keys::FileOpsKey;
use crate::file_ops::DefaultFileOpsDelegate;
use crate::file_ops::FileIgnoreResult;
use crate::file_ops::FileIgnores;
use crate::file_ops::FileOps;
use crate::file_ops::PathMetadataOrRedirection;
use crate::file_ops::SimpleDirEntry;
use crate::io::IoProvider;
use crate::legacy_configs::dice::HasLegacyConfigs;
use crate::result::SharedResult;

pub trait HasFileOps<'c> {
    type T: FileOps;
    fn file_ops(&'c self) -> Self::T;
}

pub trait FileChangeHandler {
    fn file_changed(&self, path: CellPath);
    fn file_removed(&self, path: CellPath);
    fn file_added(&self, path: CellPath);
    fn dir_added(&self, path: CellPath);
    fn dir_removed(&self, path: CellPath);
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
#[derive(Clone, Dupe)]
struct FileToken(Arc<CellPath>);

impl FileToken {
    async fn read(&self, fs: &dyn FileOps) -> anyhow::Result<String> {
        fs.read_file(&self.0).await
    }
}

#[derive(Clone, Dupe)]
pub struct DiceFileOps<'c>(pub &'c DiceComputations);

mod keys {
    use derive_more::Display;
    use gazebo::prelude::*;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
    #[display(fmt = "{:?}", self)]
    pub struct FileOpsKey();
}

async fn get_default_file_ops(dice: &DiceComputations) -> SharedResult<Arc<dyn FileOps>> {
    #[derive(Clone, Dupe, PartialEq)]
    struct DiceFileOpsDelegate {
        io: PartialEqWrapper<Arc<dyn IoProvider>>,
        cells: CellResolver,
        ignores: Arc<HashMap<CellName, FileIgnores>>,
    }

    // NOTE: We need this because derive(PartialEq) fails to compile otherwise on
    // DiceFileOpsDelegate: move occurs because `*__self_1_0` has type `std::sync::Arc<dyn
    // IoProvider>`, which does not implement the `Copy` trait.
    #[derive(Clone, Dupe, PartialEq)]
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

        fn io_provider(&self) -> &dyn IoProvider {
            self.io.as_ref()
        }
    }

    #[async_trait]
    impl Key for FileOpsKey {
        type Value = SharedResult<Arc<dyn FileOps>>;
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
                    FileIgnores::new_for_interpreter(ignore_spec, &*cell_paths, this_path)?;
                ignores.insert(cell_name.clone(), cell_ignores);
            }

            Ok(Arc::new(DiceFileOpsDelegate {
                io: PartialEqWrapper(io),
                cells,
                ignores: Arc::new(ignores),
            }))
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

    dice.compute(&FileOpsKey()).await?
}

fn panic_expected_parent(path: &CellPath) -> ! {
    panic!(
        "a file/dir in the repo must have a parent, but `{}` had none",
        path
    )
}

fn file_contents_modify(dice: &DiceComputations, path: CellPath) {
    dice.changed(Some(ReadFileKey(path.clone())));
    dice.changed(Some(PathMetadataKey(path)));
}

fn file_existence_modify(dice: &DiceComputations, path: CellPath) {
    let parent = path
        .parent()
        .unwrap_or_else(|| panic_expected_parent(&path));

    file_contents_modify(dice, path);
    dice.changed(Some(ReadDirKey(parent)));
}

fn dir_existence_modify(dice: &DiceComputations, path: CellPath) {
    let parent = path
        .parent()
        .unwrap_or_else(|| panic_expected_parent(&path));
    dice.changed(Some(PathMetadataKey(path.clone())));
    dice.changed(vec![ReadDirKey(path), ReadDirKey(parent)]);
}

impl FileChangeHandler for DiceComputations {
    fn file_changed(&self, path: CellPath) {
        file_contents_modify(self, path);
    }

    fn file_removed(&self, path: CellPath) {
        file_existence_modify(self, path)
    }

    fn file_added(&self, path: CellPath) {
        file_existence_modify(self, path)
    }

    fn dir_added(&self, path: CellPath) {
        dir_existence_modify(self, path);
    }

    fn dir_removed(&self, path: CellPath) {
        dir_existence_modify(self, path);
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "ReadFile({})", _0)]
struct ReadFileKey(CellPath);

#[async_trait]
impl Key for ReadFileKey {
    type Value = FileToken;
    async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
        FileToken(Arc::new(self.0.clone()))
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        false
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "ReadDir({})", _0)]
struct ReadDirKey(CellPath);

#[async_trait]
impl Key for ReadDirKey {
    type Value = SharedResult<Arc<Vec<SimpleDirEntry>>>;
    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        get_default_file_ops(ctx).await?.read_dir(&self.0).await
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

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "PathMetadataKey({})", _0)]
struct PathMetadataKey(CellPath);

#[async_trait]
impl Key for PathMetadataKey {
    type Value = SharedResult<Option<PathMetadataOrRedirection>>;
    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        get_default_file_ops(ctx)
            .await?
            .read_path_metadata_if_exists(&self.0)
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

#[async_trait]
impl<'c> FileOps for DiceFileOps<'c> {
    async fn read_file(&self, path: &CellPath) -> anyhow::Result<String> {
        let file_ops = get_default_file_ops(self.0).await?;

        self.0
            .compute(&ReadFileKey(path.to_owned()))
            .await?
            .read(&*file_ops)
            .await
    }

    async fn read_dir(&self, path: &CellPath) -> SharedResult<Arc<Vec<SimpleDirEntry>>> {
        self.0.compute(&ReadDirKey(path.clone())).await?
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: &CellPath,
    ) -> SharedResult<Option<PathMetadataOrRedirection>> {
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
