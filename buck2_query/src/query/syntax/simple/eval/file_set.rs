/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use anyhow::anyhow;
use buck2_core::cells::paths::CellPath;
use derive_more::Display;
use indexmap::IndexSet;
use starlark::values::display::display_container;

use crate::query::{
    environment::{QueryEnvironment, QueryTarget},
    syntax::simple::eval::{error::QueryError, set::TargetSet},
};

/// An entry in a FileSet.
#[derive(Hash, Eq, PartialEq, Debug, Clone, Display)]
pub struct FileNode(pub CellPath);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FileSet {
    files: IndexSet<FileNode>,
}

impl fmt::Display for FileSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_container(f, "[", "]", self.files.iter())
    }
}

impl FileSet {
    pub fn new(files: IndexSet<FileNode>) -> Self {
        Self { files }
    }

    pub fn union(&self, right: &FileSet) -> anyhow::Result<FileSet> {
        let mut files = self.files.clone();
        for file in right.files.iter() {
            files.insert(file.clone());
        }
        Ok(Self { files })
    }

    pub fn owner<T: QueryTarget>(
        &self,
        _env: &dyn QueryEnvironment<Target = T>,
    ) -> anyhow::Result<TargetSet<T>> {
        Err(anyhow!(QueryError::FunctionUnimplemented("owner()")))
    }

    pub fn iter(&self) -> impl Iterator<Item = &CellPath> + '_ {
        self.files.iter().map(|v| &v.0)
    }
}
