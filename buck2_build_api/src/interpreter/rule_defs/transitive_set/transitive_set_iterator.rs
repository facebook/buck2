/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

use anyhow::Context as _;
use starlark::values::Value;
use starlark::values::ValueIdentity;
use starlark::values::ValueLike;

use crate::interpreter::rule_defs::transitive_set::transitive_set::NodeGen;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetGen;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetLike;

pub trait TransitiveSetIteratorLike<'a, 'v, V>: Iterator<Item = &'a TransitiveSetGen<V>>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    fn values(self: Box<Self>) -> TransitiveSetValuesIteratorGen<'a, 'v, V>;
}

fn assert_transitive_set<'v, V>(child: Value<'v>) -> &'v TransitiveSetGen<V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
{
    TransitiveSetLike::from_value(child)
        .with_context(|| {
            format!(
                "Invalid set: expected {:?}, got: {:?}",
                std::any::type_name::<V>(),
                child
            )
        })
        .unwrap()
}

/// A DFS, left-to-right iterator over a TransitiveSet.
pub struct PreorderTransitiveSetIteratorGen<'a, 'v, V> {
    stack: Vec<&'a TransitiveSetGen<V>>,
    seen: HashSet<ValueIdentity<'v>>,
}

impl<'a, 'v, V> PreorderTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    pub fn new(set: &'a TransitiveSetGen<V>) -> Self {
        Self {
            stack: vec![set],
            seen: HashSet::new(),
        }
    }

    fn enqueue_children(&mut self, children: &'a [V]) {
        for child in children.iter().rev() {
            let child = child.to_value();

            if self.seen.insert(child.identity()) {
                self.stack.push(assert_transitive_set(child));
            }
        }
    }
}

impl<'a, 'v, V> TransitiveSetIteratorLike<'a, 'v, V> for PreorderTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    fn values(self: Box<Self>) -> TransitiveSetValuesIteratorGen<'a, 'v, V> {
        TransitiveSetValuesIteratorGen { inner: self }
    }
}

/// An iterator over values of a TransitiveSet. Notionally a FilterMap, but defined as its own type
/// since there are a few too many lifetimes involved to make a nice `impl Iterator<...>` work
/// here.
impl<'a, 'v, V> Iterator for PreorderTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    type Item = &'a TransitiveSetGen<V>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.stack.pop()?;
        self.enqueue_children(&next.children);
        Some(next)
    }
}

/// A postorder traversal iterator over a TransitiveSet.
/// Traverses by children left-to-right, and then visits the current node.
pub struct PostorderTransitiveSetIteratorGen<'a, 'v, V> {
    stack: Vec<Option<&'a TransitiveSetGen<V>>>,
    parent_stack: Vec<&'a TransitiveSetGen<V>>,
    seen: HashSet<ValueIdentity<'v>>,
}

impl<'a, 'v, V> PostorderTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    pub fn new(set: &'a TransitiveSetGen<V>) -> Self {
        Self {
            stack: vec![Some(set)],
            parent_stack: vec![],
            seen: HashSet::new(),
        }
    }

    fn enqueue_children(&mut self, children: &'a [V]) {
        for child in children.iter().rev() {
            let child = child.to_value();

            if self.seen.insert(child.identity()) {
                self.stack.push(Some(assert_transitive_set(child)));
            }
        }
    }
}

impl<'a, 'v, V> TransitiveSetIteratorLike<'a, 'v, V>
    for PostorderTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    fn values(self: Box<Self>) -> TransitiveSetValuesIteratorGen<'a, 'v, V> {
        TransitiveSetValuesIteratorGen { inner: self }
    }
}

/// An iterator over values of a TransitiveSet. Notionally a FilterMap, but defined as its own type
/// since there are a few too many lifetimes involved to make a nice `impl Iterator<...>` work
/// here.
impl<'a, 'v, V> Iterator for PostorderTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    type Item = &'a TransitiveSetGen<V>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(next) = self.stack.pop()? {
            if next.children.is_empty() {
                return Some(next);
            }

            self.stack.push(None); // Add sentinel value to indicate end of the parent list.
            self.parent_stack.push(next);
            self.enqueue_children(&next.children);
        }

        // Found a sentinel value indicating children are traversed, return parent.
        self.parent_stack.pop()
    }
}

/// A topological traversal iterator over a TransitiveSet, such that nodes are listed after all
/// nodes that have them as descendants.
///
/// This is equivalent to a pre-order traversal, except that when nodes are shared with more than
/// one parent it is returned in the order of its last occurrence.
pub struct TopologicalTransitiveSetIteratorGen<'a, 'v, V> {
    output_stack: Vec<&'a TransitiveSetGen<V>>,
    instance_counts: HashMap<ValueIdentity<'v>, u32>,
}

impl<'a, 'v, V> TopologicalTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    pub fn new(set: &'a TransitiveSetGen<V>) -> Self {
        Self {
            output_stack: vec![set],
            instance_counts: TopologicalTransitiveSetIteratorGen::count_instances(set),
        }
    }

    fn count_instances(set: &'a TransitiveSetGen<V>) -> HashMap<ValueIdentity<'v>, u32> {
        let mut stack = vec![set];
        let mut instance_counts: HashMap<ValueIdentity<'v>, u32> = HashMap::new();

        while let Some(next) = stack.pop() {
            for child in next.children.iter().rev() {
                let child = child.to_value();

                match instance_counts.entry(child.identity()) {
                    Entry::Occupied(mut o) => {
                        *o.get_mut() += 1;
                    }
                    Entry::Vacant(v) => {
                        v.insert(1);
                        stack.push(assert_transitive_set(child));
                    }
                }
            }
        }

        instance_counts
    }

    fn enqueue_children(&mut self, children: &'a [V]) {
        for child in children.iter().rev() {
            let child = child.to_value();

            // It's safe to unwrap since instance_counts is populated during construction and contains
            // all nodes in the tree. `unwrap()` would only fail if the tree was modified.
            let count: &mut u32 = self.instance_counts.get_mut(&child.identity()).unwrap();

            // If this fails, the tree either contains cycles or was modified after construction.
            assert!(*count > 0, "Unexpected node when traversing tree");

            if *count == 1 {
                // Push the last occurrence of the node onto the output stack.
                self.output_stack.push(assert_transitive_set(child));
            }

            *count -= 1;
        }
    }
}

impl<'a, 'v, V> TransitiveSetIteratorLike<'a, 'v, V>
    for TopologicalTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    fn values(self: Box<Self>) -> TransitiveSetValuesIteratorGen<'a, 'v, V> {
        TransitiveSetValuesIteratorGen { inner: self }
    }
}

/// An iterator over values of a TransitiveSet. Notionally a FilterMap, but defined as its own type
/// since there are a few too many lifetimes involved to make a nice `impl Iterator<...>` work
/// here.
impl<'a, 'v, V> Iterator for TopologicalTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    type Item = &'a TransitiveSetGen<V>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.output_stack.pop()?;
        self.enqueue_children(&next.children);
        Some(next)
    }
}

/// A breadth-first-search (BFS), left-to-right iterator over a TransitiveSet.
pub struct BfsTransitiveSetIteratorGen<'a, 'v, V> {
    queue: VecDeque<&'a TransitiveSetGen<V>>,
    seen: HashSet<ValueIdentity<'v>>,
}

impl<'a, 'v, V> BfsTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    pub fn new(set: &'a TransitiveSetGen<V>) -> Self {
        Self {
            queue: VecDeque::from(vec![set]),
            seen: HashSet::new(),
        }
    }

    fn enqueue_children(&mut self, children: &'a [V]) {
        for child in children.iter() {
            let child = child.to_value();

            if self.seen.insert(child.identity()) {
                self.queue.push_back(assert_transitive_set(child));
            }
        }
    }
}

impl<'a, 'v, V> TransitiveSetIteratorLike<'a, 'v, V> for BfsTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    fn values(self: Box<Self>) -> TransitiveSetValuesIteratorGen<'a, 'v, V> {
        TransitiveSetValuesIteratorGen { inner: self }
    }
}

/// An iterator over values of a TransitiveSet. Notionally a FilterMap, but defined as its own type
/// since there are a few too many lifetimes involved to make a nice `impl Iterator<...>` work
/// here.
impl<'a, 'v, V> Iterator for BfsTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    type Item = &'a TransitiveSetGen<V>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.queue.pop_front()?;
        self.enqueue_children(&next.children);
        Some(next)
    }
}

pub struct TransitiveSetValuesIteratorGen<'a, 'v, V> {
    inner: Box<dyn TransitiveSetIteratorLike<'a, 'v, V> + 'a>,
}

impl<'a, 'v, V> Iterator for TransitiveSetValuesIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    type Item = &'a NodeGen<V>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = self.inner.next()?;
            if let Some(node) = next.node.as_ref() {
                return Some(node);
            }
        }
    }
}
