/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::CellResolver;
use dupe::Dupe;

#[derive(Clone, Dupe, Debug, Allocative)]
pub(crate) struct InterpreterCellInfo(Arc<Data>);

#[derive(Debug, Allocative)]
struct Data {
    cell_name: BuildFileCell,
    cell_resolver: CellResolver,
}

impl InterpreterCellInfo {
    pub(crate) fn new(
        cell_name: BuildFileCell,
        cell_resolver: CellResolver,
    ) -> anyhow::Result<Self> {
        Ok(Self(Arc::new(Data {
            cell_name,
            cell_resolver,
        })))
    }

    pub(crate) fn name(&self) -> BuildFileCell {
        self.0.cell_name
    }

    pub(crate) fn cell_resolver(&self) -> &CellResolver {
        &self.0.cell_resolver
    }
}
