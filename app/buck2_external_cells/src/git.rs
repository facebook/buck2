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
use buck2_core::cells::external::GitCellSetup;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use dice::DiceComputations;

pub(crate) async fn get_file_ops_delegate(
    _ctx: &mut DiceComputations<'_>,
    _setup: &GitCellSetup,
) -> anyhow::Result<Arc<dyn FileOpsDelegate>> {
    unimplemented!()
}

pub(crate) async fn materialize_all(
    _ctx: &mut DiceComputations<'_>,
    _setup: &GitCellSetup,
) -> anyhow::Result<ProjectRelativePathBuf> {
    unimplemented!()
}
