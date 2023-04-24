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
use crate::cells::CellAliasResolver;
use crate::fs::paths::file_name::FileNameBuf;

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
    /// a list of potential buildfile names for this cell (e.g. 'BUCK', 'TARGETS',
    /// 'TARGET.v2'). The candidates are listed in priority order, buck will use
    /// the first one it encounters in a directory.
    buildfiles: Vec<FileNameBuf>,
    #[derivative(Debug = "ignore")]
    /// the aliases of this specific cell
    aliases: CellAliasResolver,
}

impl CellInstance {
    pub(crate) fn new(
        name: CellName,
        path: CellRootPathBuf,
        buildfiles: Vec<FileNameBuf>,
        aliases: CellAliasResolver,
    ) -> CellInstance {
        CellInstance(Arc::new(CellData {
            name,
            path,
            buildfiles,
            aliases,
        }))
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

    // Get the name of build files for the cell.
    #[inline]
    pub fn buildfiles(&self) -> &[FileNameBuf] {
        &self.0.buildfiles
    }

    #[inline]
    pub fn cell_alias_resolver(&self) -> &CellAliasResolver {
        &self.0.aliases
    }
}
