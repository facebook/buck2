/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{hash::Hash, iter::FromIterator};

use gazebo::prelude::*;
use indexmap::Equivalent;
use starlark::collections::SmallSet;

use crate::query::environment::LabeledNode;

#[derive(Debug, Clone, Dupe)]
pub struct LabelIndexed<T: LabeledNode>(pub T);

impl<T: LabeledNode> PartialEq for LabelIndexed<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.node_ref() == other.0.node_ref()
    }
}
impl<T: LabeledNode> Eq for LabelIndexed<T> {}
impl<T: LabeledNode> Hash for LabelIndexed<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.node_ref().hash(state)
    }
}

impl<T: LabeledNode> Ord for LabelIndexed<T>
where
    T::NodeRef: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.node_ref().cmp(other.0.node_ref())
    }
}

impl<T: LabeledNode> PartialOrd for LabelIndexed<T>
where
    T::NodeRef: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.node_ref().partial_cmp(other.0.node_ref())
    }
}

struct LabelIndexer<'a, T: LabeledNode>(&'a T::NodeRef);

impl<'a, T: LabeledNode> Equivalent<LabelIndexed<T>> for LabelIndexer<'a, T> {
    fn equivalent(&self, key: &LabelIndexed<T>) -> bool {
        self.0.eq(key.0.node_ref())
    }
}
impl<'a, T: LabeledNode> Hash for LabelIndexer<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

/// This is a Set that contains `LabeledNode`s, but for all trait operation like `Eq`, `Ord` all
/// operate only on the `NodeRef` of the `LabeledNode`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LabelIndexedSet<T: LabeledNode> {
    nodes: SmallSet<LabelIndexed<T>>,
}

impl<T: LabeledNode> LabelIndexedSet<T> {
    pub fn new() -> Self {
        Self {
            nodes: SmallSet::new(),
        }
    }

    pub fn with_capacity(n: usize) -> Self {
        Self {
            nodes: SmallSet::with_capacity(n),
        }
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn get(&self, value: &T::NodeRef) -> Option<&T> {
        self.nodes.get(&LabelIndexer(value)).map(|e| &e.0)
    }

    pub fn take(&mut self, value: &T::NodeRef) -> Option<T> {
        self.nodes.take(&LabelIndexer(value)).map(|e| e.0)
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &T> + Clone {
        self.nodes.iter().map(|e| &e.0)
    }

    #[allow(clippy::should_implement_trait)] // the std trait requires concrete or boxed iterator type
    pub fn into_iter(self) -> impl ExactSizeIterator<Item = T> {
        self.nodes.into_iter().map(|e| e.0)
    }

    pub fn insert(&mut self, value: T) -> bool {
        self.nodes.insert(LabelIndexed(value))
    }

    pub fn contains(&self, value: &T::NodeRef) -> bool {
        self.nodes.contains(&LabelIndexer(value))
    }

    pub fn get_index(&self, index: usize) -> Option<&T> {
        self.nodes.get_index(index).map(|e| &e.0)
    }

    pub fn get_index_of(&self, value: &T::NodeRef) -> Option<usize> {
        self.nodes.get_index_of(&LabelIndexer(value))
    }
}

impl<T: LabeledNode> FromIterator<T> for LabelIndexedSet<T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Self {
        let iter = iter.into_iter();
        let mut res = Self::with_capacity(iter.size_hint().0);
        for item in iter {
            res.insert(item);
        }
        res
    }
}
