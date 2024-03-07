/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;

#[derive(Clone, Debug, Allocative)]
pub struct InterpreterCellInfo {
    cell_name: BuildFileCell,
    cell_resolver: CellResolver,
    cell_alias_resolver: CellAliasResolver,
}

impl InterpreterCellInfo {
    pub(crate) fn new(
        cell_name: BuildFileCell,
        cell_resolver: CellResolver,
        cell_alias_resolver: CellAliasResolver,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            cell_name,
            cell_resolver,
            cell_alias_resolver,
        })
    }

    pub(crate) fn name(&self) -> BuildFileCell {
        self.cell_name
    }

    pub fn cell_resolver(&self) -> &CellResolver {
        &self.cell_resolver
    }

    pub fn cell_alias_resolver(&self) -> &CellAliasResolver {
        &self.cell_alias_resolver
    }
}
