/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;

use anyhow::Context as _;
use starlark::values::{ValueIdentity, ValueLike};

use crate::interpreter::rule_defs::transitive_set::{
    transitive_set::NodeGen, TransitiveSetGen, TransitiveSetLike,
};

/// A DFS, left-to-right iterator over a TransitiveSet.
pub struct TransitiveSetIteratorGen<'a, 'v, V> {
    queue: Vec<&'a TransitiveSetGen<V>>,
    seen: HashSet<ValueIdentity<'v>>,
}

impl<'a, 'v, V> TransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    pub fn new(set: &'a TransitiveSetGen<V>) -> Self {
        Self {
            queue: vec![set],
            seen: HashSet::new(),
        }
    }

    pub fn enqueue_children(&mut self, children: &'a [V]) {
        for child in children.iter().rev() {
            let child = child.to_value();

            if self.seen.insert(child.identity()) {
                let next: &TransitiveSetGen<V> = TransitiveSetLike::from_value(child)
                    .with_context(|| {
                        format!(
                            "Invalid set: expected {:?}, got: {:?}",
                            std::any::type_name::<V>(),
                            child
                        )
                    })
                    .unwrap();

                self.queue.push(next);
            }
        }
    }

    pub fn values(self) -> TransitiveSetValuesIteratorGen<'a, 'v, V> {
        TransitiveSetValuesIteratorGen { inner: self }
    }
}

/// An iterator over values of a TransitiveSet. Notionally a FiterMap, but defined as its own type
/// since there are a few too many lifetimes involved to make a nice `impl Iterator<...>` work
/// here.
impl<'a, 'v, V> Iterator for TransitiveSetIteratorGen<'a, 'v, V>
where
    V: 'v + Copy + ValueLike<'v>,
    TransitiveSetGen<V>: TransitiveSetLike<'v>,
    'v: 'a,
{
    type Item = &'a TransitiveSetGen<V>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.queue.pop()?;
        self.enqueue_children(&next.children);
        Some(next)
    }
}

pub struct TransitiveSetValuesIteratorGen<'a, 'v, V> {
    inner: TransitiveSetIteratorGen<'a, 'v, V>,
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
