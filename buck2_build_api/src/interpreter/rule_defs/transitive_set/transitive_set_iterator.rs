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
