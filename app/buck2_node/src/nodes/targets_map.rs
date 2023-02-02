/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::collections::ordered_map::OrderedMap;
use buck2_core::target::label::TargetLabel;
use buck2_core::target::name::TargetName;
use buck2_core::target::name::TargetNameRef;
use dupe::Dupe;
use starlark_map::small_map;

use crate::nodes::unconfigured::TargetNode;

#[derive(Debug, thiserror::Error)]
enum TargetsError {
    #[error("Attempted to register target {0} twice")]
    RegisteredTargetTwice(TargetLabel),
}

/// Map of target -> details of those targets within a build file.
#[derive(Debug, Clone, Allocative)]
pub struct TargetsMap {
    map: OrderedMap<TargetName, TargetNode>,
}

impl TargetsMap {
    #[inline]
    pub fn new() -> TargetsMap {
        TargetsMap {
            map: OrderedMap::new(),
        }
    }

    #[inline]
    pub fn get(&self, name: &TargetNameRef) -> Option<&TargetNode> {
        self.map.get(name)
    }

    #[inline]
    pub fn contains_key(&self, name: &TargetNameRef) -> bool {
        self.map.contains_key(name)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.map.len()
    }

    #[inline]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&TargetNameRef, &TargetNode)> {
        self.map.iter().map(|(k, v)| (k.as_ref(), v))
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
        match self.map.entry(target_node.label().name().to_owned()) {
            small_map::Entry::Vacant(o) => {
                o.insert(target_node);
                Ok(())
            }
            small_map::Entry::Occupied(_) => {
                Err(TargetsError::RegisteredTargetTwice(target_node.label().dupe()).into())
            }
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
