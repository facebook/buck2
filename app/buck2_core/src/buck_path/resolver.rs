/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use dupe::Dupe;

use crate::buck_path::path::BuckPathRef;
use crate::cells::cell_path::CellPathRef;
use crate::cells::CellResolver;
use crate::fs::project_rel_path::ProjectRelativePathBuf;

#[derive(Clone, Dupe, Allocative)]
pub struct BuckPathResolver(CellResolver);

impl BuckPathResolver {
    pub fn new(cells: CellResolver) -> Self {
        BuckPathResolver(cells)
    }

    /// Resolves a 'BuckPath' into a 'ProjectRelativePath' based on the package
    /// and cell.
    pub fn resolve(&self, path: BuckPathRef) -> anyhow::Result<ProjectRelativePathBuf> {
        Ok(self.0.resolve_package(path.package())?.join(path.path()))
    }

    pub fn resolve_cell_path(&self, path: CellPathRef) -> anyhow::Result<ProjectRelativePathBuf> {
        Ok(self.0.get(path.cell())?.path().join(path.path()))
    }
}
