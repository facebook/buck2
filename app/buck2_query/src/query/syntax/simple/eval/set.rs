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

use allocative::Allocative;
use display_container::fmt_container;
use dupe::IterDupedExt;
use fancy_regex::Regex;
use fancy_regex::RegexBuilder;
use indexmap::IndexSet;

use crate::query::environment::NodeLabel;
use crate::query::environment::QueryTarget;
use crate::query::syntax::simple::eval::error::QueryError;
use crate::query::syntax::simple::eval::file_set::FileNode;
use crate::query::syntax::simple::eval::file_set::FileSet;
use crate::query::syntax::simple::eval::label_indexed;
use crate::query::syntax::simple::eval::label_indexed::LabelIndexedSet;

#[derive(Debug, Eq, PartialEq, Clone, Allocative)]
pub struct TargetSet<T: QueryTarget> {
    targets: LabelIndexedSet<T>,
}

impl<T: QueryTarget> TargetSet<T> {
    pub fn new() -> Self {
        Self {
            targets: LabelIndexedSet::new(),
        }
    }

    pub fn with_capacity(n: usize) -> Self {
        Self {
            targets: LabelIndexedSet::with_capacity(n),
        }
    }

    pub fn insert(&mut self, value: T) -> bool {
        self.targets.insert(value)
    }

    pub fn is_empty(&self) -> bool {
        self.targets.len() == 0
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
        Err(anyhow::anyhow!(QueryError::FunctionUnimplemented(
            "labels()"
        )))
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

    pub fn iter(&self) -> Iter<T> {
        self.targets.iter()
    }

    #[allow(clippy::should_implement_trait)] // the std trait requires concrete or boxed iterator type
    pub fn into_iter(self) -> impl Iterator<Item = T> {
        self.targets.into_iter()
    }

    pub fn contains(&self, item: &T::NodeRef) -> bool {
        self.targets.contains(item)
    }

    pub fn get(&self, item: &T::NodeRef) -> Option<&T> {
        self.targets.get(item)
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

pub type Iter<'a, T> = label_indexed::Iter<'a, T>;

impl<'a, T: QueryTarget> IntoIterator for &'a TargetSet<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.targets.iter()
    }
}

impl<T: QueryTarget> Extend<T> for TargetSet<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for target in iter {
            self.targets.insert(target);
        }
    }
}

impl<'a, T: QueryTarget> Extend<&'a T> for TargetSet<T> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
        self.extend(iter.into_iter().duped())
    }
}

impl<T: QueryTarget> FromIterator<T> for TargetSet<T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Self {
        let targets = LabelIndexedSet::from_iter(iter);
        Self { targets }
    }
}

/// This contains additional TargetSet functions implemented via the core
/// functions on TargetSet itself.
impl<T: QueryTarget> TargetSet<T> {
    pub fn attrfilter(
        &self,
        attribute: &str,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<TargetSet<T>> {
        self.filter(move |node| {
            node.map_attr(attribute, |val| match val {
                None => Ok(false),
                Some(v) => T::attr_any_matches(v, &filter),
            })
        })
    }

    pub(crate) fn nattrfilter(
        &self,
        attribute: &str,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<TargetSet<T>> {
        self.filter(move |node| {
            node.map_attr(attribute, |val| match val {
                None => Ok(false),
                Some(v) => Ok(!T::attr_any_matches(v, &filter)?),
            })
        })
    }

    pub fn attrregexfilter(&self, attribute: &str, value: &str) -> anyhow::Result<TargetSet<T>> {
        let regex = Regex::new(value)?;
        let filter = move |s: &'_ str| -> anyhow::Result<bool> { Ok(regex.is_match(s)?) };
        self.attrfilter(attribute, &filter)
    }

    /// Filter targets by fully qualified name using regex partial match.
    pub fn filter_name(&self, regex: &str) -> anyhow::Result<TargetSet<T>> {
        let mut re = RegexBuilder::new(regex);
        re.delegate_dfa_size_limit(100 << 20);
        let re = re.build()?;
        self.filter(|node| Ok(re.is_match(&node.node_ref().label_for_filter())?))
    }

    pub fn kind(&self, regex: &str) -> anyhow::Result<TargetSet<T>> {
        let re = Regex::new(regex)?;
        self.filter(|node| Ok(re.is_match(&node.rule_type())?))
    }

    pub fn intersect(&self, right: &TargetSet<T>) -> anyhow::Result<TargetSet<T>> {
        self.filter(|node| Ok(right.contains(node.node_ref())))
    }

    pub fn difference(&self, right: &TargetSet<T>) -> anyhow::Result<TargetSet<T>> {
        self.filter(|node| Ok(!right.contains(node.node_ref())))
    }
}

impl<T: QueryTarget> Display for TargetSet<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_container(f, "[", "]", self.targets.iter().map(|t| t.node_ref()))
    }
}
