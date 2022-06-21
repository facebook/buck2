/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Represents the forward and backward dependencies of the computation graph

use std::collections::hash_map::DefaultHasher;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;
use std::sync::RwLock;
use std::sync::RwLockReadGuard;
use std::sync::Weak;

use async_trait::async_trait;
use gazebo::cmp::PartialEqAny;
use gazebo::dupe::Dupe;

use crate::ctx::ComputationData;
use crate::incremental::graph::GraphNodeDyn;
use crate::incremental::graph::ReadOnlyHistory;
use crate::incremental::introspection::AnyKey;
use crate::incremental::transaction_ctx::TransactionCtx;
use crate::incremental::versions::MinorVersion;
use crate::incremental::versions::VersionNumber;
use crate::incremental::versions::VersionRange;
use crate::incremental::versions::VersionRanges;

/// The dependency information stored by the core engine
#[async_trait]
pub(crate) trait Dependency: Debug + Display + Send + Sync {
    async fn recompute(
        &self,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
    ) -> (Box<dyn ComputedDependency>, Arc<dyn GraphNodeDyn>);

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
pub(crate) trait ComputedDependency: Debug + Send + Sync {
    fn get_history(&self) -> ReadOnlyHistory;

    /// converts itself into the data to be stored in deps and rdeps
    fn into_dependency(self: Box<Self>) -> Arc<dyn Dependency>;

    fn get_key_equality(&self) -> PartialEqAny;

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

pub(crate) struct VersionedDependencies {
    /// once the deps at a particular version is written, it is final and never modified
    /// this BTreeMap should also be append only.
    deps: Arc<RwLock<BTreeMap<VersionNumber, Arc<HashSet<Arc<dyn Dependency>>>>>>,
}

impl VersionedDependencies {
    pub(crate) fn new() -> Self {
        Self {
            deps: Arc::new(RwLock::new(BTreeMap::new())),
        }
    }

    #[cfg(test)]
    pub(crate) fn testing_new(
        deps: BTreeMap<VersionNumber, Arc<HashSet<Arc<dyn Dependency>>>>,
    ) -> Self {
        Self {
            deps: Arc::new(RwLock::new(deps)),
        }
    }

    pub(crate) fn last(&self) -> Option<Arc<HashSet<Arc<dyn Dependency>>>> {
        Some(self.deps.read().unwrap().iter().last()?.1.dupe())
    }

    pub(crate) fn add_deps(&self, v: VersionNumber, deps: Arc<HashSet<Arc<dyn Dependency>>>) {
        if let Some(old) = self.deps.write().unwrap().insert(v, deps.dupe()) {
            assert_eq!(
                old, deps,
                "dependency at the same version number should always be equal"
            );
        }
    }

    /// Given a range of versions that we are interested in, returns a map of version ranges to
    /// dependencies, where the keys are all the versions in the ranges of interest that correspond
    /// to dependencies which are the map values.
    /// The key is a range because there can be the same dependencies occurring at different
    /// versions.
    pub(crate) fn deps_at_versions(
        &self,
        range: &VersionRanges,
    ) -> BTreeMap<VersionRanges, Arc<HashSet<Arc<dyn Dependency>>>> {
        #[derive(PartialEq, Eq)]
        struct HashWrapper(Arc<HashSet<Arc<dyn Dependency>>>);
        /// wraps a HashSet so that we can have Hash be invariant of iteration order
        #[allow(clippy::derive_hash_xor_eq)]
        impl Hash for HashWrapper {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0
                    .iter()
                    .map(|e| {
                        let mut s = DefaultHasher::new();
                        e.hash(&mut s);
                        std::num::Wrapping(s.finish())
                    })
                    .sum::<std::num::Wrapping<u64>>()
                    .hash(state)
            }
        }

        let mut deps_to_versions: HashMap<HashWrapper, VersionRanges> = HashMap::new();

        let locked = self.deps.read().unwrap();
        let mut deps = locked.iter().peekable();

        // TODO(bobyf) we might want to optimize this to not scan the whole deps set
        while let Some((v, deps_at_version)) = deps.next() {
            let range_of_dep = if let Some((next_v, _)) = deps.peek() {
                VersionRange::bounded(*v, **next_v)
            } else {
                VersionRange::begins_with(*v)
            };

            let intersect = range.intersect_range(&range_of_dep);
            if !intersect.is_empty() {
                let ranges = deps_to_versions
                    .entry(HashWrapper(deps_at_version.dupe()))
                    .or_insert_with(VersionRanges::new);

                *ranges = ranges.union(&intersect);
            }
        }

        deps_to_versions
            .into_iter()
            .map(|(ds, vs)| (vs, ds.0))
            .collect()
    }

    pub(crate) fn debug_deps(
        &self,
    ) -> &Arc<RwLock<BTreeMap<VersionNumber, Arc<HashSet<Arc<dyn Dependency>>>>>> {
        &self.deps
    }
}

/// Eq and Hash for an rdep is related to the address of the node it points to, since in a dice
/// session, the node stored is always kept alive via an `Arc`, node equality is the ptr address
#[derive(Clone, Dupe)]
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
#[derive(Clone, Dupe)]
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
        self.rdeps.write().unwrap().insert(Rdep {
            node: dependent,
            relevant_version: current_version,
        });
    }

    pub(crate) fn rdeps(&self) -> RwLockReadGuard<HashSet<Rdep>> {
        self.rdeps.read().unwrap()
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use std::collections::BTreeMap;
    use std::collections::HashSet;
    use std::sync::Arc;
    use std::sync::RwLockReadGuard;

    use crate::incremental::graph::dependencies::VersionedDependencies;
    use crate::incremental::versions::VersionNumber;
    use crate::incremental::Dependency;

    pub(crate) trait VersionedDependenciesExt {
        fn deps<'a>(
            &'a self,
        ) -> RwLockReadGuard<'a, BTreeMap<VersionNumber, Arc<HashSet<Arc<dyn Dependency>>>>>;
    }

    impl VersionedDependenciesExt for VersionedDependencies {
        fn deps<'a>(
            &'a self,
        ) -> RwLockReadGuard<'a, BTreeMap<VersionNumber, Arc<HashSet<Arc<dyn Dependency>>>>>
        {
            self.deps.read().unwrap()
        }
    }
}
