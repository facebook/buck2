/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use buck2_core::cells::cell_path::CellPath;
use derive_more::Display;
use fancy_regex::Regex;
use gazebo::display::display_container;
use indexmap::IndexSet;

use crate::query::environment::QueryEnvironment;
use crate::query::environment::QueryTarget;
use crate::query::syntax::simple::eval::error::QueryError;
use crate::query::syntax::simple::eval::set::TargetSet;

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

    pub(crate) fn filter_name(&self, regex: &str) -> anyhow::Result<Self> {
        let re = Regex::new(regex)?;
        self.filter(|node| Ok(re.is_match(&node.0.to_string())?))
    }

    fn filter<F: Fn(&FileNode) -> anyhow::Result<bool>>(&self, filter: F) -> anyhow::Result<Self> {
        let mut files = IndexSet::new();
        for file in self.files.iter() {
            if filter(file)? {
                files.insert(file.clone());
            }
        }
        Ok(Self { files })
    }

    pub fn insert_all(&mut self, other: &FileSet) {
        for v in other.files.iter() {
            self.files.insert(v.clone());
        }
    }

    pub fn union(&self, right: &FileSet) -> FileSet {
        let mut files = self.files.clone();
        for file in right.files.iter() {
            files.insert(file.clone());
        }
        Self { files }
    }

    pub fn owner<T: QueryTarget>(
        &self,
        _env: &dyn QueryEnvironment<Target = T>,
    ) -> anyhow::Result<TargetSet<T>> {
        Err(anyhow::anyhow!(QueryError::FunctionUnimplemented(
            "owner()"
        )))
    }

    pub fn iter(&self) -> impl Iterator<Item = &CellPath> + '_ {
        self.files.iter().map(|v| &v.0)
    }

    pub fn get_index(&self, index: usize) -> Option<&CellPath> {
        match self.files.get_index(index) {
            Some(x) => Some(&x.0),
            None => None,
        }
    }
}
