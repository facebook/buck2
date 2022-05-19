/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Trackers that records dependencies and reverse dependencies during execution of requested nodes

use std::{
    collections::HashSet,
    sync::{Arc, Mutex},
};

use gazebo::prelude::*;

use crate::incremental::{
    dep_trackers::internals::ComputedDep,
    graph::{dependencies::ComputedDependency, GraphNode, GraphNodeDyn},
    versions::VersionNumber,
    IncrementalComputeProperties, IncrementalEngine,
};

/// The 'DepsTracker' is used to record dependencies of a particular compute node by calling
/// 'record' for each dependency, and then getting a list of 'Dependency's at the end by calling
/// 'collect_deps'.
struct RecordingDepsTracker {
    deps: HashSet<Box<dyn ComputedDependency>>,
}

impl RecordingDepsTracker {
    fn new() -> Self {
        Self {
            deps: HashSet::new(),
        }
    }

    /// records k with the given evaluator and engine
    fn record<K>(
        &mut self,
        k: K::Key,
        v: VersionNumber,
        engine: Arc<IncrementalEngine<K>>,
        node: GraphNode<K>,
    ) where
        K: IncrementalComputeProperties,
    {
        self.deps.insert(box ComputedDep {
            engine: Arc::downgrade(&engine),
            k_v: (k, v),
            node,
        });
    }

    fn collect_deps(self) -> HashSet<Box<dyn ComputedDependency>> {
        self.deps
    }
}

struct RecordingRdepsTracker {
    rdeps: Vec<Arc<dyn GraphNodeDyn>>,
}

impl RecordingRdepsTracker {
    fn new() -> Self {
        Self { rdeps: Vec::new() }
    }

    fn record(&mut self, dep: Arc<dyn GraphNodeDyn>) {
        self.rdeps.push(dep)
    }

    fn collect_rdeps(self) -> Vec<Arc<dyn GraphNodeDyn>> {
        self.rdeps
    }
}

struct BothRecordingDepTrackers {
    deps: RecordingDepsTracker,
    rdeps: RecordingRdepsTracker,
}

#[derive(Default)]
pub(crate) struct BothDeps {
    pub(crate) deps: HashSet<Box<dyn ComputedDependency>>,
    pub(crate) rdeps: Vec<Arc<dyn GraphNodeDyn>>,
}

impl BothDeps {
    pub(crate) fn only_one_dep<S: IncrementalComputeProperties>(
        key: S::Key,
        version: VersionNumber,
        node: GraphNode<S>,
        incremental_engine: &Arc<IncrementalEngine<S>>,
    ) -> BothDeps {
        let dep: Box<dyn ComputedDependency> = box ComputedDep::<S> {
            engine: Arc::downgrade(incremental_engine),
            k_v: (key, version),
            node: node.dupe(),
        };
        BothDeps {
            deps: HashSet::from_iter([dep]),
            rdeps: Vec::from_iter([node.into_dyn()]),
        }
    }
}

enum BothDepTrackersImpl {
    Noop,
    Recording(Mutex<BothRecordingDepTrackers>),
}

pub(crate) struct BothDepTrackers(BothDepTrackersImpl);

/// There are two variants, a 'Recording' tracker and a 'Noop' tracker. The 'Noop' tracker never
/// tracks any dependencies such that 'collect_deps' is always empty. The 'Recording' tracker will
/// actually track the dependencies.
impl BothDepTrackers {
    pub(crate) fn noop() -> BothDepTrackers {
        BothDepTrackers(BothDepTrackersImpl::Noop)
    }

    pub(crate) fn recording() -> BothDepTrackers {
        BothDepTrackers(BothDepTrackersImpl::Recording(Mutex::new(
            BothRecordingDepTrackers {
                deps: RecordingDepsTracker::new(),
                rdeps: RecordingRdepsTracker::new(),
            },
        )))
    }

    /// records k with the given evaluator and engine
    pub(crate) fn record<K>(
        &self,
        k: K::Key,
        v: VersionNumber,
        engine: Arc<IncrementalEngine<K>>,
        node: GraphNode<K>,
    ) where
        K: IncrementalComputeProperties,
    {
        match &self.0 {
            BothDepTrackersImpl::Noop => {}
            BothDepTrackersImpl::Recording(recording) => {
                let mut recording = recording.lock().unwrap();
                let BothRecordingDepTrackers { deps, rdeps } = &mut *recording;
                deps.record(k, v, engine, node.dupe());
                rdeps.record(node.into_dyn());
            }
        }
    }

    pub(crate) fn collect_deps(self) -> BothDeps {
        match self.0 {
            BothDepTrackersImpl::Noop => BothDeps::default(),
            BothDepTrackersImpl::Recording(recording) => {
                let BothRecordingDepTrackers { deps, rdeps } = recording.into_inner().unwrap();
                let deps = deps.collect_deps();
                let rdeps = rdeps.collect_rdeps();
                BothDeps { deps, rdeps }
            }
        }
    }
}

mod internals {
    use std::{
        any::type_name,
        fmt,
        fmt::{Debug, Display, Formatter},
        hash::{Hash, Hasher},
        sync::{Arc, Weak},
    };

    use async_trait::async_trait;
    use gazebo::{cmp::PartialEqAny, prelude::*};

    /// internal representation of dependencies recorded by the trackers
    use crate::incremental::{
        introspection::AnyKey, versions::VersionNumber, ComputedDependency, Dependency,
        IncrementalComputeProperties, IncrementalEngine,
    };
    use crate::{
        ctx::ComputationData,
        incremental::{
            graph::{GraphNode, GraphNodeDyn, ReadOnlyHistory, VersionedGraphKeyRef},
            transaction_ctx::TransactionCtx,
            versions::MinorVersion,
        },
    };

    pub(crate) struct ComputedDep<K: IncrementalComputeProperties> {
        pub(crate) engine: Weak<IncrementalEngine<K>>,
        pub(crate) k_v: (K::Key, VersionNumber),
        pub(crate) node: GraphNode<K>,
    }

    impl<K> ComputedDependency for ComputedDep<K>
    where
        K: IncrementalComputeProperties,
    {
        fn get_history(&self) -> ReadOnlyHistory {
            self.node.get_history()
        }

        fn into_dependency(self: Box<Self>) -> Arc<dyn Dependency> {
            Arc::new(Dep {
                engine: self.engine,
                k: self.k_v.0,
            })
        }

        fn get_key_equality(&self) -> PartialEqAny {
            PartialEqAny::new(&self.k_v)
        }

        fn hash(&self, mut state: &mut dyn Hasher) {
            self.k_v.hash(&mut state);
        }

        fn is_valid(&self) -> bool {
            self.node.is_valid()
        }
    }

    impl<K> Debug for ComputedDep<K>
    where
        K: IncrementalComputeProperties,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
            write!(
                f,
                "ComputedDependency(({:?}={:?}) -> {:?}, version={:?})",
                type_name::<K::Key>(),
                self.k_v.0,
                type_name::<K::Value>(),
                self.k_v.1
            )
        }
    }

    pub(crate) struct Dep<K>
    where
        K: IncrementalComputeProperties,
    {
        pub(crate) engine: Weak<IncrementalEngine<K>>,
        pub(crate) k: K::Key,
    }

    impl<K> Dep<K>
    where
        K: IncrementalComputeProperties,
    {
        pub(crate) fn engine(&self) -> Arc<IncrementalEngine<K>> {
            self.engine.upgrade().expect(
                "IncrementalEngine should not be destroyed because IncrementalEngine owns Dep",
            )
        }
    }

    #[async_trait]
    impl<K> Dependency for Dep<K>
    where
        K: IncrementalComputeProperties,
    {
        async fn recompute(
            &self,
            transaction_ctx: &Arc<TransactionCtx>,
            extra: &ComputationData,
        ) -> (Box<dyn ComputedDependency>, Arc<dyn GraphNodeDyn>) {
            let res = K::recompute(&self.k, &self.engine(), transaction_ctx, extra).await;

            (
                box ComputedDep {
                    engine: self.engine.dupe(),
                    k_v: (self.k.clone(), transaction_ctx.get_version()),
                    node: res.dupe(),
                },
                res.into_dyn(),
            )
        }

        fn lookup_node(&self, v: VersionNumber, mv: MinorVersion) -> Option<Arc<dyn GraphNodeDyn>> {
            if let Some(node) = self
                .engine()
                .versioned_cache
                .get(VersionedGraphKeyRef::new(v, &self.k), mv)
                .unpack_match()
            {
                Some(node.dupe().into_dyn())
            } else {
                None
            }
        }

        fn dirty(&self, v: VersionNumber) {
            self.engine().dirty(self.k.clone(), v, false)
        }

        fn get_key_equality(&self) -> PartialEqAny {
            PartialEqAny::new(&self.k)
        }

        fn hash(&self, mut state: &mut dyn Hasher) {
            self.k.hash(&mut state)
        }

        fn introspect(&self) -> AnyKey {
            AnyKey::new(self.k.clone())
        }
    }

    impl<K> Debug for Dep<K>
    where
        K: IncrementalComputeProperties,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
            write!(
                f,
                "Dependency(({:?}={:?}) -> {:?})",
                type_name::<K::Key>(),
                self.k,
                type_name::<K::Value>()
            )
        }
    }

    impl<K> Display for Dep<K>
    where
        K: IncrementalComputeProperties,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "Dependency({})", self.k)
        }
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use std::sync::{Arc, Weak};

    // re-export internals for testing
    pub(crate) use crate::incremental::dep_trackers::internals::{ComputedDep, Dep};
    use crate::incremental::{
        graph::{GraphNode, OccupiedGraphNode},
        versions::VersionNumber,
        IncrementalComputeProperties, IncrementalEngine,
    };

    pub(crate) trait DepExt<K: IncrementalComputeProperties> {
        fn testing_new(engine: Weak<IncrementalEngine<K>>, k: K::Key) -> Self;
    }

    impl<K> DepExt<K> for Dep<K>
    where
        K: IncrementalComputeProperties,
    {
        fn testing_new(engine: Weak<IncrementalEngine<K>>, k: K::Key) -> Self {
            Dep { engine, k }
        }
    }

    pub(crate) trait ComputedDepExt<K: IncrementalComputeProperties> {
        fn testing_new(
            engine: Weak<IncrementalEngine<K>>,
            k_v: (K::Key, VersionNumber),
            node: Arc<OccupiedGraphNode<K>>,
        ) -> Self;
    }

    impl<K> ComputedDepExt<K> for ComputedDep<K>
    where
        K: IncrementalComputeProperties,
    {
        fn testing_new(
            engine: Weak<IncrementalEngine<K>>,
            k_v: (K::Key, VersionNumber),
            node: Arc<OccupiedGraphNode<K>>,
        ) -> Self {
            ComputedDep {
                engine,
                k_v,
                node: GraphNode::occupied(node),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use gazebo::prelude::*;

    use crate::{
        ctx::{testing::ComputationDataExt, ComputationData},
        incremental::{
            dep_trackers::{BothDeps, RecordingDepsTracker, RecordingRdepsTracker},
            evaluator::testing::{EvaluatorFn, EvaluatorUnreachable},
            graph::OccupiedGraphNode,
            history::CellHistory,
            testing::ComputedDependencyExt,
            versions::VersionNumber,
            IncrementalEngine, TransactionCtx,
        },
        ValueWithDeps,
    };

    #[test]
    fn recording_rdeps_tracker_tracks_rdeps() {
        let mut rdeps_tracker = RecordingRdepsTracker::new();

        let node = Arc::new(OccupiedGraphNode::<EvaluatorFn<usize, usize>>::new(
            1337,
            2,
            CellHistory::verified(VersionNumber::new(0)),
        ));
        rdeps_tracker.record(node.dupe());
        let tracked = rdeps_tracker.collect_rdeps();

        assert_eq!(tracked.len(), 1);
    }

    #[tokio::test]
    async fn recording_deps_tracker_tracks_deps() {
        let mut deps_tracker = RecordingDepsTracker::new();
        // set up so that we have keys 2 and 3 with a history of VersionNumber(1)
        let fn_for_2_and_3 = |k| ValueWithDeps {
            value: k,
            both_deps: BothDeps::default(),
        };

        let engine = IncrementalEngine::new(EvaluatorFn::new(async move |k| fn_for_2_and_3(k)));

        let ctx = Arc::new(TransactionCtx::testing_new(VersionNumber::new(1)));

        let node1 = engine
            .eval_entry_versioned(&2, &ctx, ComputationData::testing_new())
            .await;
        let node2 = engine
            .eval_entry_versioned(&3, &ctx, ComputationData::testing_new())
            .await;

        deps_tracker.record(2, VersionNumber::new(1), engine.dupe(), node1);
        deps_tracker.record(3, VersionNumber::new(1), engine.dupe(), node2);

        let deps = deps_tracker.collect_deps();

        let expected = hashset![
            ComputedDependencyExt::<EvaluatorUnreachable<_, i32>>::testing_raw(
                2,
                VersionNumber::new(1),
                true
            ),
            ComputedDependencyExt::<EvaluatorUnreachable<_, i32>>::testing_raw(
                3,
                VersionNumber::new(1),
                true
            ),
        ];
        assert_eq!(deps, expected);
    }
}
