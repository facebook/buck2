/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::dice::file_ops::delegate::FileOpsDelegate;
use buck2_common::file_ops::RawDirEntry;
use buck2_common::file_ops::RawPathMetadata;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_external_cells_bundled::get_bundled_data;
use buck2_external_cells_bundled::BundledCell;
use cmp_any::PartialEqAny;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;

pub(crate) fn find_bundled_data(cell_name: CellName) -> anyhow::Result<BundledCell> {
    #[derive(buck2_error::Error, Debug)]
    #[error("No bundled cell named `{0}`, options are `{}`", _1.join(", "))]
    struct CellNotBundled(String, Vec<&'static str>);

    let cell_name = cell_name.as_str();

    get_bundled_data()
        .iter()
        .find(|data| data.name == cell_name)
        .copied()
        .ok_or_else(|| {
            CellNotBundled(
                cell_name.to_owned(),
                get_bundled_data()
                    .iter()
                    .filter(|data| !data.is_testing)
                    .map(|data| data.name)
                    .collect(),
            )
            .into()
        })
}

#[derive(allocative::Allocative, PartialEq, Eq)]
pub(crate) struct BundledFileOpsDelegate {}

#[async_trait::async_trait]
#[allow(clippy::todo)]
impl FileOpsDelegate for BundledFileOpsDelegate {
    async fn read_file_if_exists(
        &self,
        _path: &'async_trait CellRelativePath,
    ) -> anyhow::Result<Option<String>> {
        todo!()
    }

    /// Return the list of file outputs, sorted.
    async fn read_dir(
        &self,
        _path: &'async_trait CellRelativePath,
    ) -> anyhow::Result<Vec<RawDirEntry>> {
        todo!()
    }

    async fn read_path_metadata_if_exists(
        &self,
        _path: &'async_trait CellRelativePath,
    ) -> anyhow::Result<Option<RawPathMetadata>> {
        todo!()
    }

    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }
}

fn get_file_ops_delegate_impl(_data: BundledCell) -> anyhow::Result<BundledFileOpsDelegate> {
    Ok(BundledFileOpsDelegate {})
}

pub(crate) async fn get_file_ops_delegate(
    ctx: &mut DiceComputations<'_>,
    cell_name: CellName,
) -> anyhow::Result<Arc<BundledFileOpsDelegate>> {
    #[derive(
        dupe::Dupe,
        Clone,
        Copy,
        Debug,
        derive_more::Display,
        PartialEq,
        Eq,
        Hash,
        allocative::Allocative
    )]
    struct BundledFileOpsDelegateKey(CellName);

    #[async_trait::async_trait]
    impl Key for BundledFileOpsDelegateKey {
        type Value = buck2_error::Result<Arc<BundledFileOpsDelegate>>;

        async fn compute(
            &self,
            _dice: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            let data = find_bundled_data(self.0)?;
            Ok(Arc::new(get_file_ops_delegate_impl(data)?))
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            // No need for non-trivial equality, because this has no deps and is never recomputed
            false
        }
    }

    Ok(ctx.compute(&BundledFileOpsDelegateKey(cell_name)).await??)
}
