/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::collections::hash_map::Entry;

use buck2_error::internal_error;
use buck2_util::hash::BuckHasherBuilder;
use starlark::values::Value;
use starlark::values::ValueIdentity;
use starlark::values::ValueLike;

use crate::interpreter::rule_defs::transitive_set::TransitiveSetGen;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetLike;
use crate::interpreter::rule_defs::transitive_set::transitive_set::NodeGen;

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
        .ok_or_else(|| {
            internal_error!(
                "Invalid set: expected {:?}, got: {:?}",
                std::any::type_name::<V>(),
                child
            )
        })
        .unwrap()
}

/// Preorder depth-first traversal, visiting parent node first, then children in an unspecified
/// order that minimizes memory usage during traversal.
pub struct PreorderTransitiveSetIteratorGen<'a, 'v, V: ValueLike<'v>> {
    stack: Vec<&'a TransitiveSetGen<V>>,
    seen: HashSet<ValueIdentity<'v>, BuckHasherBuilder>,
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
            seen: Default::default(),
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

/// Postorder depth-first traversal, visiting children left-to-right before visiting their parent
/// node.
pub struct PostorderTransitiveSetIteratorGen<'a, 'v, V: ValueLike<'v>> {
    stack: Vec<(&'a TransitiveSetGen<V>, PostorderMark<'v>)>,
    seen: HashSet<ValueIdentity<'v>, BuckHasherBuilder>,
}

impl<'a, 'v, V> PostorderTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    pub fn new(set: &'a TransitiveSetGen<V>) -> Self {
        let mut iterator = Self {
            stack: vec![(set, PostorderMark::Ready)],
            seen: Default::default(),
        };
        iterator.enqueue_children(&set.children);
        iterator
    }

    fn enqueue_children(&mut self, children: &'a [V]) {
        for child in children.iter().rev() {
            let child = child.to_value();
            self.stack.push((
                assert_transitive_set(child),
                PostorderMark::Pending(child.identity()),
            ));
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

enum PostorderMark<'v> {
    /// When the stack returns to this position, children have been visited.
    Ready,
    /// The stack may return to this position with some children not yet having
    /// been visited. Check `seen`.
    Pending(ValueIdentity<'v>),
}

impl<'a, 'v, V> Iterator for PostorderTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    type Item = &'a TransitiveSetGen<V>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.stack.pop()? {
                (tset, PostorderMark::Ready) => return Some(tset),
                (tset, PostorderMark::Pending(identity)) => {
                    if self.seen.insert(identity) {
                        self.stack.push((tset, PostorderMark::Ready));
                        self.enqueue_children(&tset.children);
                    }
                }
            }
        }
    }
}

/// Topological sort order, such that nodes are visited after all nodes that have them as
/// descendants.
///
/// This is similar to the pre-order traversal, except that when nodes are shared with more than one
/// parent it is returned in the order of its last occurrence.
pub struct TopologicalTransitiveSetIteratorGen<'a, 'v, V: ValueLike<'v>> {
    output_stack: Vec<&'a TransitiveSetGen<V>>,
    instance_counts: HashMap<ValueIdentity<'v>, u32, BuckHasherBuilder>,
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

    fn count_instances(
        set: &'a TransitiveSetGen<V>,
    ) -> HashMap<ValueIdentity<'v>, u32, BuckHasherBuilder> {
        let mut stack = vec![set];
        let mut instance_counts = HashMap::<ValueIdentity<'v>, u32, BuckHasherBuilder>::default();

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

/// Preorder breadth-first-search (BFS), visits parent node, then eagerly visits all children
/// left-to-right before traversing to any grandchildren.
pub struct BfsTransitiveSetIteratorGen<'a, 'v, V: ValueLike<'v>> {
    queue: VecDeque<&'a TransitiveSetGen<V>>,
    seen: HashSet<ValueIdentity<'v>, BuckHasherBuilder>,
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
            seen: Default::default(),
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

/// Preorder depth-first-search (DFS).
///
/// This is similar to the pre-order traversal, except that children are guaranteed to be visited
/// left-to-right.
pub struct DfsTransitiveSetIteratorGen<'a, 'v, V: ValueLike<'v>> {
    stack: Vec<(&'a TransitiveSetGen<V>, Option<ValueIdentity<'v>>)>,
    seen: HashSet<ValueIdentity<'v>, BuckHasherBuilder>,
}

impl<'a, 'v, V> DfsTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    pub fn new(set: &'a TransitiveSetGen<V>) -> Self {
        Self {
            stack: vec![(set, None)],
            seen: Default::default(),
        }
    }
}

impl<'a, 'v, V> TransitiveSetIteratorLike<'a, 'v, V> for DfsTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    fn values(self: Box<Self>) -> TransitiveSetValuesIteratorGen<'a, 'v, V> {
        TransitiveSetValuesIteratorGen { inner: self }
    }
}

impl<'a, 'v, V> Iterator for DfsTransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    type Item = &'a TransitiveSetGen<V>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (tset, identity) = self.stack.pop()?;
            if identity.is_none_or(|id| self.seen.insert(id)) {
                for child in tset.children.iter().rev() {
                    let child = child.to_value();
                    let child_identity = child.identity();
                    if !self.seen.contains(&child_identity) {
                        self.stack
                            .push((assert_transitive_set(child), Some(child_identity)));
                    }
                }
                return Some(tset);
            }
        }
    }
}

/// An iterator over values of a TransitiveSet. Notionally a FilterMap, but defined as its own type
/// since there are a few too many lifetimes involved to make a nice `impl Iterator<...>` work
/// here.
pub struct TransitiveSetValuesIteratorGen<'a, 'v, V: ValueLike<'v>> {
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
