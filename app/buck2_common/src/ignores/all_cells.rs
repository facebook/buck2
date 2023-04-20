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

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::name::CellName;
use buck2_core::cells::unchecked_cell_rel_path::UncheckedCellRelativePath;
use dice::DiceComputations;
use itertools::Itertools;

use crate::dice::cells::HasCellResolver;
use crate::ignores::file_ignores::FileIgnoreResult;
use crate::ignores::file_ignores::FileIgnores;
use crate::legacy_configs::dice::HasLegacyConfigs;

/// Ignored path configurations for all cells.
#[derive(Allocative, Debug, Eq, PartialEq)]
pub(crate) struct AllCellIgnores {
    ignores: HashMap<CellName, FileIgnores>,
}

impl AllCellIgnores {
    pub(crate) fn check_ignored(
        &self,
        cell: CellName,
        path: &UncheckedCellRelativePath,
    ) -> anyhow::Result<FileIgnoreResult> {
        Ok(self
            .ignores
            .get(&cell)
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "Internal error: Should've had an ignore spec for `{}`. Had `{}`",
                    cell,
                    self.ignores.keys().join(", ")
                )
            })?
            .check(path))
    }
}

#[async_trait]
pub(crate) trait HasAllCellIgnores {
    async fn new_all_cell_ignores(&self) -> anyhow::Result<Arc<AllCellIgnores>>;
}

#[async_trait]
impl HasAllCellIgnores for DiceComputations {
    async fn new_all_cell_ignores(&self) -> anyhow::Result<Arc<AllCellIgnores>> {
        let cells = self.get_cell_resolver().await?;
        let configs = self.get_legacy_configs_on_dice().await?;

        let cell_paths: Vec<_> = cells.cells().map(|e| (e.1.name(), e.1.path())).collect();
        let mut ignores = HashMap::new();

        for (cell_name, instance) in cells.cells() {
            let this_path = instance.path();
            let config = configs.get(cell_name).unwrap();
            let ignore_spec = config.get("project", "ignore")?;
            let ignore_spec = ignore_spec.as_ref().map_or("", |s| &**s);

            let cell_ignores =
                FileIgnores::new_for_interpreter(ignore_spec, &cell_paths, this_path)?;
            ignores.insert(cell_name, cell_ignores);
        }

        Ok(Arc::new(AllCellIgnores { ignores }))
    }
}
