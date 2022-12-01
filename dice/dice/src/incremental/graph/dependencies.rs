/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Represents the forward and backward dependencies of the computation graph

use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;
use std::sync::Weak;

use allocative::Allocative;
use async_trait::async_trait;
use gazebo::cmp::PartialEqAny;
use gazebo::dupe::Dupe;
use parking_lot::RwLock;
use parking_lot::RwLockReadGuard;

use crate::ctx::ComputationData;
use crate::incremental::graph::GraphNodeDyn;
use crate::incremental::graph::ReadOnlyHistory;
use crate::incremental::transaction_ctx::TransactionCtx;
use crate::incremental::versions::MinorVersion;
use crate::incremental::versions::VersionNumber;
use crate::introspection::graph::AnyKey;
use crate::DiceResult;

/// The dependency information stored by the core engine
#[async_trait]
pub(crate) trait Dependency: Allocative + Debug + Display + Send + Sync {
    async fn recompute(
        &self,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
    ) -> DiceResult<(Box<dyn ComputedDependency>, Arc<dyn GraphNodeDyn>)>;

    /// looks up the stored node of this dependency. This can return `None` if this entry
    /// was evicted from the storage.
    fn lookup_node(&self, v: VersionNumber, mv: MinorVersion) -> Option<Arc<dyn GraphNodeDyn>>;

    fn dirty(&self, v: VersionNumber);

    fn get_key_equality(&self) -> PartialEqAny;

    fn hash(&self, state: &mut dyn Hasher);

    /// Provide a type-erased AnyKey representing this Dependency. This is used when traversing
    /// DICE to dump its state.
    fn introspect(&self) -> AnyKey;
}

impl PartialEq for dyn Dependency {
    fn eq(&self, other: &Self) -> bool {
        self.get_key_equality() == other.get_key_equality()
    }
}

impl Eq for dyn Dependency {}

impl Hash for dyn Dependency {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash(state)
    }
}

/// The dependency information right after they were requested. This allows us to look up
/// information from the dependency without further computation.
pub(crate) trait ComputedDependency: Allocative + Debug + Send + Sync {
    fn get_history(&self) -> ReadOnlyHistory;

    /// converts itself into the data to be stored in deps and rdeps
    fn into_dependency(self: Box<Self>) -> Box<dyn Dependency>;

    fn get_key_equality(&self) -> (PartialEqAny, VersionNumber);

    fn hash(&self, state: &mut dyn Hasher);

    fn is_valid(&self) -> bool;
}

impl PartialEq for dyn ComputedDependency {
    fn eq(&self, other: &Self) -> bool {
        self.get_key_equality() == other.get_key_equality()
    }
}

impl Eq for dyn ComputedDependency {}

impl Hash for dyn ComputedDependency {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash(state)
    }
}

#[derive(Allocative)]
pub(crate) struct VersionedDependencies {
    /// once the deps at a particular version is written, it is final and never modified
    /// We only store the dependencies relevant to the most recent result
    deps: RwLock<Option<(VersionNumber, Arc<Vec<Box<dyn Dependency>>>)>>,
}

impl VersionedDependencies {
    pub(crate) fn new() -> Self {
        Self {
            deps: RwLock::new(None),
        }
    }

    pub(crate) fn deps(&self) -> Option<Arc<Vec<Box<dyn Dependency>>>> {
        self.deps.read().as_ref().map(|d| d.1.dupe())
    }

    pub(crate) fn add_deps(&self, v: VersionNumber, deps: Arc<Vec<Box<dyn Dependency>>>) {
        let mut this_deps = self.deps.write();
        if this_deps.as_ref().map_or(true, |d| v > d.0) {
            // we only ever write the newest version of the dependencies of this node for simplicity
            // That way, if we are ever dirtied, we just check if the latest version of the deps
            // have changed at the dirtied version which only requires spawning one set of deps.
            // It might cause us to falsely fail to reuse some nodes, but this is less memory
            // and less work per node when in incremental cases.
            *this_deps = Some((v, deps));
        }
    }

    pub(crate) fn debug_deps(
        &self,
    ) -> &RwLock<Option<(VersionNumber, Arc<Vec<Box<dyn Dependency>>>)>> {
        &self.deps
    }
}

/// Eq and Hash for an rdep is related to the address of the node it points to, since in a dice
/// session, the node stored is always kept alive via an `Arc`, node equality is the ptr address
#[derive(Clone, Dupe, Allocative)]
pub(crate) struct Rdep {
    /// the node that this rdep points to
    pub(crate) node: Weak<dyn GraphNodeDyn>,
    /// the version for which this rdep was recorded
    pub(crate) relevant_version: VersionNumber,
}

impl PartialEq for Rdep {
    fn eq(&self, other: &Self) -> bool {
        self.relevant_version == other.relevant_version && Weak::ptr_eq(&self.node, &other.node)
    }
}

impl Eq for Rdep {}

impl Hash for Rdep {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.relevant_version.hash(state);
        self.node.upgrade().map(|p| Arc::as_ptr(&p)).hash(state)
    }
}

// the set of reverse dependencies of a node
#[derive(Clone, Dupe, Allocative)]
pub(crate) struct VersionedRevDependencies {
    // TODO(bobyf) do we need something special for quick lookup per version or is this fine
    rdeps: Arc<RwLock<HashSet<Rdep>>>,
}

impl VersionedRevDependencies {
    pub(crate) fn new() -> Self {
        Self {
            rdeps: Arc::new(Default::default()),
        }
    }

    pub(crate) fn add_rdep(
        &self,
        dependent: Weak<dyn GraphNodeDyn>,
        current_version: VersionNumber,
    ) {
        self.rdeps.write().insert(Rdep {
            node: dependent,
            relevant_version: current_version,
        });
    }

    pub(crate) fn rdeps(&self) -> RwLockReadGuard<HashSet<Rdep>> {
        self.rdeps.read()
    }
}
