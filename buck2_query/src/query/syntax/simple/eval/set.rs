/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use anyhow::anyhow;
use fancy_regex::Regex;
use gazebo::display::display_container;
use indexmap::IndexSet;

use crate::query::environment::QueryTarget;
use crate::query::environment::QueryTargetAttr;
use crate::query::syntax::simple::eval::error::QueryError;
use crate::query::syntax::simple::eval::file_set::FileNode;
use crate::query::syntax::simple::eval::file_set::FileSet;
use crate::query::syntax::simple::eval::label_indexed::LabelIndexedSet;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TargetSet<T: QueryTarget> {
    targets: LabelIndexedSet<T>,
}

impl<T: QueryTarget> TargetSet<T> {
    pub fn new() -> Self {
        Self {
            targets: LabelIndexedSet::new(),
        }
    }

    pub fn insert(&mut self, value: T) -> bool {
        self.targets.insert(value)
    }

    pub fn insert_all(&mut self, other: &TargetSet<T>) {
        for v in other.iter() {
            self.insert(v.dupe());
        }
    }

    pub fn len(&self) -> usize {
        self.targets.len()
    }

    fn filter<F: Fn(&T) -> anyhow::Result<bool>>(&self, filter: F) -> anyhow::Result<TargetSet<T>> {
        let mut targets = LabelIndexedSet::new();
        for target in self.targets.iter() {
            if filter(target)? {
                targets.insert(target.dupe());
            }
        }
        Ok(Self { targets })
    }

    pub fn buildfile(&self) -> FileSet {
        let mut files = IndexSet::new();
        for target in self.targets.iter() {
            files.insert(FileNode(target.buildfile_path().path()));
        }
        FileSet::new(files)
    }

    pub fn inputs(&self) -> anyhow::Result<FileSet> {
        let mut files = IndexSet::new();
        for target in self.targets.iter() {
            target.inputs_for_each(|file| {
                files.insert(FileNode(file));
                anyhow::Ok(())
            })?;
        }
        Ok(FileSet::new(files))
    }

    // TODO(cjhopman): Does this even make sense?
    // TODO(cjhopman): I think this needs a heap to allocate values
    pub fn labels(&self, _attr: &str) -> anyhow::Result<()> {
        Err(anyhow!(QueryError::FunctionUnimplemented("labels()")))
    }

    pub fn union(&self, right: &TargetSet<T>) -> TargetSet<T> {
        let mut targets = LabelIndexedSet::new();
        for target in self.targets.iter() {
            targets.insert(target.dupe());
        }
        for target in right.targets.iter() {
            targets.insert(target.dupe());
        }
        Self { targets }
    }

    pub fn iter_names(&self) -> impl Iterator<Item = &T::NodeRef> + Clone {
        self.targets.iter().map(|e| e.node_ref())
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.targets.iter()
    }

    #[allow(clippy::should_implement_trait)] // the std trait requires concrete or boxed iterator type
    pub fn into_iter(self) -> impl Iterator<Item = T> {
        self.targets.into_iter()
    }

    pub fn contains(&self, item: &T::NodeRef) -> bool {
        self.targets.contains(item)
    }

    pub fn get_index(&self, index: usize) -> Option<&T> {
        self.targets.get_index(index)
    }

    pub fn get_index_of(&self, item: &T::NodeRef) -> Option<usize> {
        self.targets.get_index_of(item)
    }

    pub fn last(&self) -> Option<&T> {
        self.targets.last()
    }
}

/// This contains additional TargetSet functions implemented via the core
/// functions on TargetSet itself.
pub trait TargetSetExt {
    type T: QueryTarget;

    fn filter<F: Fn(&Self::T) -> anyhow::Result<bool>>(
        &self,
        filter: F,
    ) -> anyhow::Result<TargetSet<Self::T>>;

    fn attrfilter(
        &self,
        attribute: &str,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<TargetSet<Self::T>> {
        self.filter(move |node| {
            node.map_attr(attribute, |val| match val {
                None => Ok(false),
                Some(v) => v.any_matches(&filter),
            })
        })
    }

    fn nattrfilter(
        &self,
        attribute: &str,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<TargetSet<Self::T>> {
        self.filter(move |node| {
            node.map_attr(attribute, |val| match val {
                None => Ok(false),
                Some(v) => Ok(!v.any_matches(&filter)?),
            })
        })
    }

    fn attrregexfilter(&self, attribute: &str, value: &str) -> anyhow::Result<TargetSet<Self::T>> {
        let regex = Regex::new(value)?;
        let filter = move |s: &'_ str| -> anyhow::Result<bool> { Ok(regex.is_match(s)?) };
        self.attrfilter(attribute, &filter)
    }

    fn filter_name(&self, regex: &str) -> anyhow::Result<TargetSet<Self::T>> {
        let re = Regex::new(regex)?;
        self.filter(|node| Ok(re.is_match(&node.node_ref().to_string())?))
    }

    fn kind(&self, regex: &str) -> anyhow::Result<TargetSet<Self::T>> {
        let re = Regex::new(regex)?;
        self.filter(|node| Ok(re.is_match(&node.rule_type())?))
    }

    fn intersect(&self, right: &TargetSet<Self::T>) -> anyhow::Result<TargetSet<Self::T>> {
        self.filter(|node| Ok(right.contains(node.node_ref())))
    }

    fn difference(&self, right: &TargetSet<Self::T>) -> anyhow::Result<TargetSet<Self::T>> {
        self.filter(|node| Ok(!right.contains(node.node_ref())))
    }
}

impl<T: QueryTarget> TargetSetExt for TargetSet<T> {
    type T = T;

    fn filter<F: Fn(&T) -> anyhow::Result<bool>>(&self, filter: F) -> anyhow::Result<TargetSet<T>> {
        TargetSet::filter(self, filter)
    }
}

impl<T: QueryTarget> Display for TargetSet<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_container(f, "[", "]", self.targets.iter().map(|t| t.node_ref()))
    }
}
