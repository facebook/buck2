/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::ops::Deref;

use allocative::Allocative;
use ref_cast::RefCast;

use crate::cells::paths::CellRelativePath;
use crate::fs::project::ProjectRelativePath;
use crate::fs::project::ProjectRelativePathBuf;

/// Path to the cell root.
#[derive(RefCast, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct CellRootPath(ProjectRelativePath);

impl CellRootPath {
    /// Constructor. Does not check the path is a valid cell root.
    pub fn new(path: &ProjectRelativePath) -> &CellRootPath {
        CellRootPath::ref_cast(path)
    }

    /// Project relative path to the cell root.
    pub fn as_project_relative_path(&self) -> &ProjectRelativePath {
        &self.0
    }

    /// Join cell path and cell-relative path.
    pub fn join(&self, path: &CellRelativePath) -> ProjectRelativePathBuf {
        self.0.join(path)
    }

    /// To owned.
    pub fn to_buf(&self) -> CellRootPathBuf {
        CellRootPathBuf(self.0.to_buf())
    }
}

impl Deref for CellRootPath {
    type Target = ProjectRelativePath;

    fn deref(&self) -> &ProjectRelativePath {
        &self.0
    }
}

impl AsRef<ProjectRelativePath> for CellRootPath {
    fn as_ref(&self) -> &ProjectRelativePath {
        &self.0
    }
}

/// Path to the cell root.
#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::Display, Allocative)]
#[display(fmt = "{}", .0)]
pub struct CellRootPathBuf(ProjectRelativePathBuf);

impl CellRootPathBuf {
    /// Constructor. Does not check the path is a valid cell root.
    pub fn new(path: ProjectRelativePathBuf) -> Self {
        CellRootPathBuf(path)
    }

    /// Project relative path to the cell root.
    pub fn project_relative_path(&self) -> &ProjectRelativePath {
        &self.0
    }
}

impl Deref for CellRootPathBuf {
    type Target = CellRootPath;

    fn deref(&self) -> &CellRootPath {
        CellRootPath::new(&self.0)
    }
}

impl Borrow<CellRootPath> for CellRootPathBuf {
    fn borrow(&self) -> &CellRootPath {
        self.deref()
    }
}
