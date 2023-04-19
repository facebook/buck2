/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use buck2_core::collections::ordered_set;
use buck2_core::collections::ordered_set::OrderedSet;
use buck2_core::target::label::TargetLabel;
use buck2_core::target::name::TargetNameRef;
use buck2_util::indent::indent;
use dupe::Dupe;

use crate::nodes::unconfigured::TargetNode;

#[derive(Debug, thiserror::Error)]
enum TargetsError {
    #[error(
        "Attempted to register target {0} twice, {}",
        Self::format_call_stack_for_registered_target_twice(_1)
    )]
    RegisteredTargetTwice(TargetLabel, Option<String>),
}

impl TargetsError {
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

#[derive(Debug, Clone, Dupe, Allocative)]
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
#[derive(Debug, Clone, Allocative)]
pub struct TargetsMap {
    map: OrderedSet<NameIndexed>,
}

impl TargetsMap {
    #[inline]
    pub fn new() -> TargetsMap {
        TargetsMap {
            map: OrderedSet::new(),
        }
    }

    #[inline]
    pub fn get(&self, name: &TargetNameRef) -> Option<&TargetNode> {
        self.map.get(name).map(|NameIndexed(n)| n)
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
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&TargetNameRef, &TargetNode)> {
        self.map.iter().map(|NameIndexed(n)| (n.label().name(), n))
    }

    #[inline]
    pub fn keys(&self) -> impl ExactSizeIterator<Item = &TargetNameRef> {
        self.iter().map(|(k, _)| k)
    }

    #[inline]
    pub fn values(&self) -> impl ExactSizeIterator<Item = &TargetNode> {
        self.iter().map(|(_, v)| v)
    }

    #[inline]
    pub fn record(&mut self, target_node: TargetNode) -> anyhow::Result<()> {
        match self.map.try_insert(NameIndexed(target_node)) {
            Err(ordered_set::OccupiedError {
                value: NameIndexed(target_node),
                occupied,
            }) => Err(TargetsError::RegisteredTargetTwice(
                target_node.label().dupe(),
                occupied.0.call_stack(),
            )
            .into()),
            Ok(()) => Ok(()),
        }
    }
}

/// For tests.
impl FromIterator<TargetNode> for TargetsMap {
    fn from_iter<T: IntoIterator<Item = TargetNode>>(iter: T) -> Self {
        let mut map = TargetsMap::new();
        for target_node in iter {
            map.record(target_node).unwrap();
        }
        map
    }
}
