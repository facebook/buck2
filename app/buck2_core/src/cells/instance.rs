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
use derivative::Derivative;
use dupe::Dupe;

use crate::cells::cell_root_path::CellRootPath;
use crate::cells::cell_root_path::CellRootPathBuf;
use crate::cells::name::CellName;
use crate::cells::nested::NestedCells;
use crate::cells::CellAliasResolver;

#[derive(Debug, buck2_error::Error)]
enum CellInstanceError {
    #[error("Inconsistent cell name: `{0}` in instance, but `{1}` in alias resolver")]
    InconsistentCellName(CellName, CellName),
}

/// A 'CellInstance', contains a 'CellName' and a path for that cell.
#[derive(Clone, Debug, derive_more::Display, Dupe, PartialEq, Eq, Allocative)]
#[display(fmt = "{}", "_0.name")]
pub struct CellInstance(Arc<CellData>);

#[derive(Derivative, PartialEq, Eq, Allocative)]
#[derivative(Debug)]
struct CellData {
    /// the fully canonicalized 'CellName'
    name: CellName,
    /// the project relative path to this 'CellInstance'
    path: CellRootPathBuf,
    #[derivative(Debug = "ignore")]
    /// the aliases of this specific cell
    aliases: CellAliasResolver,
    nested_cells: NestedCells,
}

impl CellInstance {
    pub(crate) fn new(
        name: CellName,
        path: CellRootPathBuf,
        aliases: CellAliasResolver,
        nested_cells: NestedCells,
    ) -> anyhow::Result<CellInstance> {
        if name != aliases.current {
            return Err(CellInstanceError::InconsistentCellName(name, aliases.current).into());
        }
        Ok(CellInstance(Arc::new(CellData {
            name,
            path,
            aliases,
            nested_cells,
        })))
    }

    /// Get the name of the cell, as supplied in `cell_name//foo:bar`.
    #[inline]
    pub fn name(&self) -> CellName {
        self.0.name.dupe()
    }

    /// Get the path of the cell, where it is routed.
    #[inline]
    pub fn path(&self) -> &CellRootPath {
        &self.0.path
    }

    #[inline]
    pub fn testing_cell_alias_resolver(&self) -> &CellAliasResolver {
        &self.0.aliases
    }

    #[inline]
    pub fn non_external_cell_alias_resolver(&self) -> &CellAliasResolver {
        &self.0.aliases
    }

    #[inline]
    pub fn nested_cells(&self) -> &NestedCells {
        &self.0.nested_cells
    }
}
