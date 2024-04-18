/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]

use async_trait::async_trait;
use buck2_core::cells::name::CellName;

mod bundled;

struct ConcreteExternalCellsImpl;

#[async_trait]
impl buck2_common::external_cells::ExternalCellsImpl for ConcreteExternalCellsImpl {
    fn check_bundled_cell_exists(&self, cell_name: CellName) -> anyhow::Result<()> {
        bundled::find_bundled_data(cell_name).map(|_| ())
    }
}

pub fn init_late_bindings() {
    buck2_common::external_cells::EXTERNAL_CELLS_IMPL.init(&ConcreteExternalCellsImpl);
}
