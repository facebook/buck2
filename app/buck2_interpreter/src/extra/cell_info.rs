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
use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use dupe::Dupe;

#[derive(Clone, Dupe, Debug, Allocative)]
pub struct InterpreterCellInfo(Arc<Data>);

#[derive(Debug, Allocative)]
struct Data {
    cell_name: BuildFileCell,
    cell_resolver: CellResolver,
    default_visibility_to_public: bool,
}

impl InterpreterCellInfo {
    pub fn new(
        cell_name: BuildFileCell,
        config: &dyn LegacyBuckConfigView,
        cell_resolver: CellResolver,
    ) -> anyhow::Result<Self> {
        // TODO(nga): move this to dice
        let default_visibility_to_public = config
            .parse("buildfile", "buck2_default_visibility_to_public")?
            .unwrap_or(false);

        Ok(Self(Arc::new(Data {
            cell_name,
            cell_resolver,
            default_visibility_to_public,
        })))
    }

    pub fn name(&self) -> BuildFileCell {
        self.0.cell_name
    }

    pub fn cell_alias_resolver(&self) -> anyhow::Result<&CellAliasResolver> {
        Ok(self
            .0
            .cell_resolver
            .get(self.0.cell_name.name())?
            .cell_alias_resolver())
    }

    pub fn default_visibility_to_public(&self) -> bool {
        self.0.default_visibility_to_public
    }
}
