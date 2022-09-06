/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::CellAliasResolver;
use gazebo::prelude::*;

#[derive(Clone, Dupe, Debug)]
pub struct InterpreterCellInfo(Arc<Data>);

#[derive(Debug)]
struct Data {
    cell_name: BuildFileCell,
    cell_alias_resolver: CellAliasResolver,
    default_visibility_to_public: bool,
}

impl InterpreterCellInfo {
    pub fn new(
        cell_name: BuildFileCell,
        config: &dyn LegacyBuckConfigView,
        cell_alias_resolver: CellAliasResolver,
    ) -> anyhow::Result<Self> {
        // TODO(nga): move this to dice
        let default_visibility_to_public = config
            .parse("buildfile", "buck2_default_visibility_to_public")?
            .unwrap_or(false);

        Ok(Self(Arc::new(Data {
            cell_name,
            cell_alias_resolver,
            default_visibility_to_public,
        })))
    }

    pub fn name(&self) -> &BuildFileCell {
        &self.0.cell_name
    }

    pub fn cell_alias_resolver(&self) -> &CellAliasResolver {
        &self.0.cell_alias_resolver
    }

    pub fn default_visibility_to_public(&self) -> bool {
        self.0.default_visibility_to_public
    }
}
