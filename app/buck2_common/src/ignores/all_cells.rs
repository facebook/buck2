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

use async_trait::async_trait;
use buck2_core::cells::name::CellName;
use dice::DiceComputations;
use dupe::Dupe;

use crate::dice::cells::HasCellResolver;
use crate::ignores::file_ignores::CellFileIgnores;
use crate::ignores::ignore_set::HasIgnoreSet;

#[async_trait]
pub(crate) trait HasCellFileIgnores {
    async fn new_cell_ignores(
        &mut self,
        cell_name: CellName,
    ) -> buck2_error::Result<Arc<CellFileIgnores>>;
}

#[async_trait]
impl HasCellFileIgnores for DiceComputations<'_> {
    async fn new_cell_ignores(
        &mut self,
        cell_name: CellName,
    ) -> buck2_error::Result<Arc<CellFileIgnores>> {
        let cells = self.get_cell_resolver().await?;
        // Validate that the cell exists before constructing.
        let _ = cells.get(cell_name)?;
        let ignores = self.global_data().get_ignore_set();
        Ok(Arc::new(CellFileIgnores::new(
            cells.dupe(),
            cell_name,
            ignores,
        )))
    }
}
