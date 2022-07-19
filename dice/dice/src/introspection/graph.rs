/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Eq;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use gazebo::cmp::PartialEqAny;
use gazebo::prelude::*;
use serde::Deserialize;
use serde::Serialize;

#[derive(PartialEq, Eq, Hash, Serialize, Deserialize, Clone, Dupe, Copy)]
#[serde(transparent)]
pub struct KeyID(pub usize);

#[derive(PartialEq, Eq, Hash, Serialize, Deserialize, Clone, Dupe, Copy)]
#[serde(transparent)]
pub struct NodeID(pub usize);

#[derive(
    PartialEq,
    Eq,
    Hash,
    Serialize,
    Deserialize,
    Clone,
    Dupe,
    Copy,
    Ord,
    PartialOrd
)]
#[serde(transparent)]
pub struct VersionNumber(pub usize);

#[derive(Serialize, Deserialize)]
pub enum GraphNodeKind {
    Occupied,
    Transient,
    Vacant,
}

#[derive(Serialize, Deserialize)]
pub struct CellHistory {
    pub verified: BTreeSet<VersionNumber>,
    /// versions of dirty, mapping ot whether or not it's a forced dirty (which means recompute
    /// regardless of node changed)
    pub dirtied: BTreeMap<VersionNumber, bool>,
}

#[derive(Serialize, Deserialize)]
pub struct SerializedGraphNode {
    pub node_id: NodeID,
    pub kind: GraphNodeKind,
    pub history: CellHistory,
    /// Deps and Rdeps are behind read locks, and if dumping after a panic
    /// it's theoretically possible for those locks to be poisoned.
    /// Therefore, they're optional.
    pub deps: Option<BTreeMap<VersionNumber, HashSet<KeyID>>>,
    pub rdeps: Option<Vec<(VersionNumber, Option<NodeID>)>>,
}

#[derive(Serialize, Deserialize)]
pub struct SerializedGraphNodesForKey {
    pub id: KeyID,
    pub key: String,
    pub type_name: String,
    pub nodes: BTreeMap<VersionNumber, Option<SerializedGraphNode>>,
}

pub trait EngineForIntrospection {
    fn keys<'a>(&'a self) -> Box<dyn Iterator<Item = AnyKey> + 'a>;
    fn edges<'a>(&'a self) -> Box<dyn Iterator<Item = (AnyKey, Vec<AnyKey>)> + 'a>;
    fn nodes<'a>(
        &'a self,
        keys: &'a mut HashMap<String, KeyID>,
    ) -> Box<dyn Iterator<Item = SerializedGraphNodesForKey> + 'a>;
    fn len_for_introspection(&self) -> usize;
}

pub trait KeyForIntrospection: Display + 'static {
    fn get_key_equality(&self) -> PartialEqAny;

    fn hash(&self, state: &mut dyn Hasher);

    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

impl<K> KeyForIntrospection for K
where
    K: Display + Hash + Eq + 'static,
{
    fn get_key_equality(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn hash(&self, mut state: &mut dyn Hasher) {
        K::hash(self, &mut state)
    }
}

pub struct AnyKey {
    pub inner: Box<dyn KeyForIntrospection>,
}

/// Shorten a type name like
/// ```ignore
/// <dice::ctx::DiceComputations as buck2_interpreter::extra::buckconfig::HasLegacyBuckConfigForStarlark>
///     ::get_legacy_buck_config_for_starlark::{{closure}}::LegacyBuckConfigForStarlarkKey
/// ```
/// to
/// `LegacyBuckConfigForStarlarkKey`.
pub fn short_type_name(type_name: &str) -> &str {
    type_name
        .rsplit("::")
        .next()
        .filter(|s| {
            // TODO(nga): shorten types like `Vec<String>`.
            //   Ignore them for now.
            !s.contains('>')
        })
        .unwrap_or(type_name)
}

impl AnyKey {
    pub fn new(k: impl KeyForIntrospection) -> Self {
        Self { inner: box k }
    }

    pub fn type_name(&self) -> &'static str {
        self.inner.type_name()
    }

    pub fn short_type_name(&self) -> &'static str {
        short_type_name(self.type_name())
    }
}

impl Display for AnyKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl PartialEq for AnyKey {
    fn eq(&self, other: &Self) -> bool {
        self.inner.get_key_equality() == other.inner.get_key_equality()
    }
}

impl Eq for AnyKey {}

impl Hash for AnyKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

#[cfg(test)]
mod tests {
    use std::any::type_name;

    use crate::introspection::graph::short_type_name;

    #[test]
    fn test_short_type_name() {
        assert_eq!("String", short_type_name(type_name::<String>()));

        let closure = || {
            struct MyLocalTypeInClosure;
            short_type_name(type_name::<MyLocalTypeInClosure>())
        };
        assert_eq!("MyLocalTypeInClosure", closure());

        // We do not shorten generic types yet.
        assert_eq!(
            type_name::<Vec<String>>(),
            short_type_name(type_name::<Vec<String>>())
        );
    }
}
