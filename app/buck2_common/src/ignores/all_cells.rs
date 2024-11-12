/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::cells::name::CellName;
use dice::DiceComputations;

use crate::dice::cells::HasCellResolver;
use crate::ignores::file_ignores::CellFileIgnores;
use crate::legacy_configs::dice::HasLegacyConfigs;
use crate::legacy_configs::key::BuckconfigKeyRef;

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
        let instance = cells.get(cell_name)?;
        let config = self.get_legacy_config_on_dice(cell_name).await?;

        let ignore_spec = config.lookup(
            self,
            BuckconfigKeyRef {
                section: "project",
                property: "ignore",
            },
        )?;
        let ignore_spec = ignore_spec.as_ref().map_or("", |s| &**s);

        let cell_ignores = CellFileIgnores::new_for_interpreter(
            ignore_spec,
            instance.nested_cells().clone(),
            cells.is_root_cell(cell_name),
        )?;

        Ok(Arc::new(cell_ignores))
    }
}
