/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Borrow;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use buck2_core::target::label::label::TargetLabel;
use buck2_core::target::name::TargetNameRef;
use buck2_hash::BuckFixedInsertionSet;
use buck2_hash::BuckIndexSet;
use buck2_util::indent::indent;
use dupe::Dupe;
use pagable::Pagable;

use crate::nodes::unconfigured::TargetNode;
use crate::nodes::unconfigured::TargetNodeRef;

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
pub enum TargetsMapRecordError {
    #[error(
        "Attempted to register target {0} twice, {}",
        TargetsMapRecordError::format_call_stack_for_registered_target_twice(_1)
    )]
    RegisteredTargetTwice(TargetLabel, Option<String>),
}

impl TargetsMapRecordError {
    fn format_call_stack_for_registered_target_twice(call_stack: &Option<String>) -> String {
        // With current macro setup, duplicate target errors occur often
        // when suffixed targets are used. When we finally fix this suffixed target setup,
        // this code can be removed.
        match call_stack {
            None => "re-run the command with `--stack` \
                    to obtain a call stack of the first registration"
                .to_owned(),
            Some(call_stack) => format!("first registered at:\n{}", indent("  ", call_stack)),
        }
    }
}

#[derive(Debug, Clone, Dupe, Allocative, Pagable)]
struct NameIndexed(TargetNode);

impl PartialEq for NameIndexed {
    fn eq(&self, other: &Self) -> bool {
        self.0.label().name() == other.0.label().name()
    }
}

impl Eq for NameIndexed {}

impl Hash for NameIndexed {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.label().name().hash(state);
    }
}

impl Borrow<TargetNameRef> for NameIndexed {
    fn borrow(&self) -> &TargetNameRef {
        self.0.label().name()
    }
}

/// Map of target -> details of those targets within a build file.
///
/// Iterates in declaration order.
#[derive(Debug, Clone, Allocative, Pagable)]
pub struct TargetsMap {
    map: BuckFixedInsertionSet<NameIndexed>,
}

impl TargetsMap {
    #[inline]
    pub fn get<'a>(&'a self, name: &TargetNameRef) -> Option<TargetNodeRef<'a>> {
        self.map.get(name).map(|NameIndexed(n)| n.as_ref())
    }

    #[inline]
    pub fn contains_key(&self, name: &TargetNameRef) -> bool {
        self.map.contains(name)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.map.len()
    }

    #[inline]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&TargetNameRef, TargetNodeRef<'_>)> {
        self.map
            .iter()
            .map(|NameIndexed(n)| (n.label().name(), n.as_ref()))
    }

    #[inline]
    pub fn keys(&self) -> impl ExactSizeIterator<Item = &TargetNameRef> {
        self.iter().map(|(k, _)| k)
    }

    #[inline]
    pub fn key_target_labels(&self) -> impl ExactSizeIterator<Item = &TargetLabel> {
        self.map.iter().map(|NameIndexed(n)| n.label())
    }

    #[inline]
    pub fn values(&self) -> impl ExactSizeIterator<Item = TargetNodeRef<'_>> {
        self.iter().map(|(_, v)| v)
    }
}

/// Collects the targets a build file declares, in declaration order, into a [`TargetsMap`].
#[derive(Debug, Default)]
pub struct TargetsMapBuilder {
    map: BuckIndexSet<NameIndexed>,
}

impl TargetsMapBuilder {
    #[inline]
    pub fn new() -> TargetsMapBuilder {
        TargetsMapBuilder::default()
    }

    #[inline]
    pub fn contains_key(&self, name: &TargetNameRef) -> bool {
        self.map.contains(name)
    }

    #[inline]
    pub fn record(&mut self, target_node: TargetNode) -> Result<(), TargetsMapRecordError> {
        let label = target_node.label().dupe();
        if self.map.insert(NameIndexed(target_node)) {
            return Ok(());
        }
        let NameIndexed(first) = self.map.get(label.name()).unwrap();
        Err(TargetsMapRecordError::RegisteredTargetTwice(
            label,
            first.call_stack(),
        ))
    }

    #[inline]
    pub fn build(self) -> TargetsMap {
        TargetsMap {
            map: self.map.freeze(),
        }
    }
}

/// For tests.
impl FromIterator<TargetNode> for TargetsMap {
    fn from_iter<T: IntoIterator<Item = TargetNode>>(iter: T) -> Self {
        let mut builder = TargetsMapBuilder::new();
        for target_node in iter {
            builder.record(target_node).unwrap();
        }
        builder.build()
    }
}
