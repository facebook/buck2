/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The value layer: interning (`docs/incrementality.md` Appendix A), retention, and the page-out
//! lifecycle. Every mutating call re-checks the consistency of values, index and core.

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use pagable::Pagable;
use pagable::pagable_typetag;

use crate::DiceKeyDyn;
use crate::api::computations::DiceComputations;
use crate::api::key::InvalidationSourcePriority;
use crate::api::key::Key;
use crate::api::key::NoValueSerialize;
use crate::api::key::ValueSerialize;
use crate::api::storage_type::StorageType;
use crate::core::graph::ValueUpdate;
use crate::core::graph::VersionedGraph;
use crate::core::graph::types::Candidate;
use crate::core::graph::types::VersionedGraphKey;
use crate::core::graph::types::VersionedGraphResult;
use crate::deps::graph::DepEdge;
use crate::deps::graph::SeriesParallelDeps;
use crate::dice::PagableNodeCounts;
use crate::key::DiceKey;
use crate::updater::ChangeType;
use crate::value::DiceComputedValue;
use crate::value::DiceKeyValue;
use crate::value::DiceValidValue;
use crate::value::MaybeResident;
use crate::value::TrackedInvalidationPaths;
use crate::versions::VersionNumber;

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct K;

#[async_trait]
impl Key for K {
    type Value = usize;

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        unimplemented!("test")
    }

    fn equality_behavior() -> crate::EqualityBehavior<Self::Value> {
        crate::EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

#[derive(Allocative, Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[pagable_typetag(DiceKeyDyn)]
struct NoEquality;

#[async_trait]
impl Key for NoEquality {
    type Value = usize;

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        unimplemented!("test")
    }

    fn equality_behavior() -> crate::EqualityBehavior<Self::Value> {
        crate::EqualityBehavior::Compare(|_, _| panic!("equality must not be consulted"))
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

fn value(v: usize) -> DiceValidValue {
    DiceValidValue::testing_new(DiceKeyValue::<K>::new(v))
}

fn key(i: u32) -> DiceKey {
    DiceKey { index: i }
}

/// A graph with the injected key `key(0)` at value 100 (version 2).
fn with_leaf() -> VersionedGraph {
    let mut graph = VersionedGraph::new();
    inject(&mut graph, key(0), 100);
    graph
}

fn inject(graph: &mut VersionedGraph, k: DiceKey, v: usize) -> VersionNumber {
    let version = graph.commit([(
        k,
        ChangeType::UpdateValue(value(v), StorageType::Injected),
        InvalidationSourcePriority::Normal,
    )]);
    graph.assert_consistent();
    version
}

fn dirty(graph: &mut VersionedGraph, k: DiceKey) -> VersionNumber {
    let version = graph.commit([(
        k,
        ChangeType::Invalidate,
        InvalidationSourcePriority::Normal,
    )]);
    graph.assert_consistent();
    version
}

/// The deps a compute of a key over `key(0)` records at `v`.
fn deps_on_leaf(graph: &VersionedGraph, v: VersionNumber) -> SeriesParallelDeps {
    let revision = graph
        .get(VersionedGraphKey::new(v, key(0)))
        .unpack_match()
        .expect("the leaf is injected")
        .0
        .revision()
        .expect("an injected value is valid");
    SeriesParallelDeps::serial_from_edges(vec![DepEdge::new(key(0), revision)])
}

/// A computed write of `value` for `k`, stamped as a transaction at `v` would stamp it.
fn compute(
    graph: &mut VersionedGraph,
    k: DiceKey,
    v: VersionNumber,
    value: DiceValidValue,
) -> DiceComputedValue {
    let deps = deps_on_leaf(graph, v);
    let epsilon = match graph.get(VersionedGraphKey::new(v, k)) {
        VersionedGraphResult::Match { epsilon, .. }
        | VersionedGraphResult::Unknown { epsilon, .. } => epsilon,
    };
    let out = graph.update(
        VersionedGraphKey::new(v, k),
        ValueUpdate::Computed {
            value,
            deps,
            epsilon,
        },
        TrackedInvalidationPaths::clean(),
    );
    graph.assert_consistent();
    out
}

fn revalidate(
    graph: &mut VersionedGraph,
    k: DiceKey,
    v: VersionNumber,
    candidate: Candidate,
) -> DiceComputedValue {
    let out = graph.update(
        VersionedGraphKey::new(v, k),
        ValueUpdate::DependencyValidated { candidate },
        TrackedInvalidationPaths::clean(),
    );
    graph.assert_consistent();
    out
}

fn candidate_at(graph: &VersionedGraph, k: DiceKey, v: VersionNumber) -> Candidate {
    match graph.get(VersionedGraphKey::new(v, k)) {
        VersionedGraphResult::Unknown {
            candidate: Some(candidate),
            ..
        } => candidate,
        other => panic!("expected an Unknown with a candidate, got {other:?}"),
    }
}

fn page_out(graph: &mut VersionedGraph, k: DiceKey, data_key: u128) {
    let to_evict: Vec<_> = graph
        .keys_to_page_out()
        .into_iter()
        .filter(|(candidate, _)| *candidate == k)
        .map(|(k, v)| {
            (
                k,
                crate::value::PageOutResult {
                    serialized_value: v,
                    data_key: pagable::DataKey::testing_new(data_key),
                },
            )
        })
        .collect();
    assert!(!to_evict.is_empty(), "{k:?} has no page-out candidate");
    graph.evict_keys(to_evict);
    graph.assert_consistent();
}

/// A write that is a trace of the same circumstances as the stored certificate reuses the stored
/// value and revision without consulting `Key::equality`.
#[test]
fn identical_certificate_reuses_stored_value_without_equality() {
    let mut graph = with_leaf();
    let v2 = graph.head();
    let first_value = DiceValidValue::testing_new(DiceKeyValue::<NoEquality>::new(1));
    let first = compute(&mut graph, key(1), v2, first_value.dupe());
    let second_value = DiceValidValue::testing_new(DiceKeyValue::<NoEquality>::new(2));
    let second = compute(&mut graph, key(1), v2, second_value);
    assert_eq!(second.revision(), first.revision());
    assert!(second.testing_resident_value().instance_equal(&first_value));
}

/// The same over a stored value that is paged out: the incoming value takes its place.
#[test]
fn identical_certificate_over_paged_out_value_makes_it_resident() {
    let mut graph = with_leaf();
    let v2 = graph.head();
    let first = compute(&mut graph, key(1), v2, value(7));
    page_out(&mut graph, key(1), 0x1234);
    assert!(
        graph
            .get(VersionedGraphKey::new(v2, key(1)))
            .unpack_match()
            .unwrap()
            .0
            .paged_out_data_key()
            .is_some()
    );
    let recomputed = value(7);
    let second = compute(&mut graph, key(1), v2, recomputed.dupe());
    assert_eq!(second.revision(), first.revision());
    assert!(second.testing_resident_value().instance_equal(&recomputed));
    assert!(
        graph
            .get(VersionedGraphKey::new(v2, key(1)))
            .unpack_match()
            .unwrap()
            .0
            .testing_resident_value()
            .instance_equal(&recomputed)
    );
    assert_eq!(graph.pagable_node_counts().paged_out, 0);
}

/// Recomputing to an equal value under different deps re-finds the revision (Appendix A step 2).
#[test]
fn recompute_to_equal_value_reuses_revision() {
    let mut graph = with_leaf();
    let v2 = graph.head();
    let first = compute(&mut graph, key(1), v2, value(42));
    let v3 = inject(&mut graph, key(0), 200);
    let again = value(42);
    let second = compute(&mut graph, key(1), v3, again.dupe());
    assert_eq!(second.revision(), first.revision());
    assert!(
        !second.testing_resident_value().instance_equal(&again),
        "the stored instance is canonical"
    );
}

#[test]
fn recompute_to_distinct_value_mints_fresh_revision() {
    let mut graph = with_leaf();
    let v2 = graph.head();
    let first = compute(&mut graph, key(1), v2, value(1));
    let v3 = inject(&mut graph, key(0), 200);
    let second = compute(&mut graph, key(1), v3, value(2));
    assert_ne!(first.revision(), second.revision());
    // The old value is no longer named by any claim and is dropped.
    assert_eq!(graph.pagable_node_counts().resident, 1);
}

/// A paged-out value cannot be compared, so a recompute over it mints; over-distinguishing is
/// sound and only costs reuse.
#[test]
fn recompute_over_paged_out_value_mints_fresh_revision() {
    let mut graph = with_leaf();
    let v2 = graph.head();
    let first = compute(&mut graph, key(1), v2, value(7));
    page_out(&mut graph, key(1), 0x1234);
    let v3 = inject(&mut graph, key(0), 200);
    let second = compute(&mut graph, key(1), v3, value(7));
    assert_ne!(first.revision(), second.revision());
}

/// Injected values are all retained, so a key returned to an earlier value gets that value's
/// revision back, and an injection of the current value is not a change.
#[test]
fn injected_values_refind_their_revisions() {
    let mut graph = VersionedGraph::new();
    let revision_at = |graph: &VersionedGraph, v| {
        graph
            .get(VersionedGraphKey::new(v, key(0)))
            .unpack_match()
            .unwrap()
            .0
            .revision()
    };
    let v2 = inject(&mut graph, key(0), 42);
    let v3 = inject(&mut graph, key(0), 43);
    let v4 = inject(&mut graph, key(0), 42);
    assert_eq!(v2, VersionNumber::testing_new(2));
    assert_eq!(v4, VersionNumber::testing_new(4));
    assert_ne!(revision_at(&graph, v2), revision_at(&graph, v3));
    assert_eq!(revision_at(&graph, v2), revision_at(&graph, v4));
    let (r2, r3) = (revision_at(&graph, v2), revision_at(&graph, v3));
    assert_eq!(
        inject(&mut graph, key(0), 42),
        v4,
        "an equal injection mints no version"
    );
    assert_eq!(revision_at(&graph, v2), r2);
    assert_eq!(revision_at(&graph, v3), r3);
    assert_eq!(revision_at(&graph, v4), r2);
}

/// A computed key with no tracked deps is a function of its untracked input alone, so two
/// computes under the same ε are the same trace: the second adopts the first's revision and value
/// without consulting equality (Appendix A step 1). A dirty ends that trace.
#[test]
fn a_dep_less_recompute_under_the_same_epsilon_adopts_the_stored_revision() {
    let mut graph = VersionedGraph::new();
    let v1 = graph.head();
    let compute_without_deps = |graph: &mut VersionedGraph, v: VersionNumber, n: usize| {
        let epsilon = match graph.get(VersionedGraphKey::new(v, key(1))) {
            VersionedGraphResult::Match { epsilon, .. }
            | VersionedGraphResult::Unknown { epsilon, .. } => epsilon,
        };
        let out = graph.update(
            VersionedGraphKey::new(v, key(1)),
            ValueUpdate::Computed {
                value: DiceValidValue::testing_new(DiceKeyValue::<NoEquality>::new(n)),
                deps: SeriesParallelDeps::None,
                epsilon,
            },
            TrackedInvalidationPaths::clean(),
        );
        graph.assert_consistent();
        out
    };
    let first = compute_without_deps(&mut graph, v1, 1);
    let second = compute_without_deps(&mut graph, v1, 2);
    assert_eq!(first.revision(), second.revision());
    #[allow(ambiguous_wide_pointer_comparisons)]
    let same_allocation = std::sync::Arc::ptr_eq(
        first.testing_resident_value().testing_value(),
        second.testing_resident_value().testing_value(),
    );
    assert!(same_allocation);
    let v2 = dirty(&mut graph, key(1));
    match graph.get(VersionedGraphKey::new(v2, key(1))) {
        VersionedGraphResult::Unknown { candidate, .. } => {
            assert!(candidate.is_none_or(|c| !c.revalidatable));
        }
        other => panic!("a dirty ends the trace, got {other:?}"),
    }
}

/// A re-issued certificate whose value a later write replaced keeps its own revision, so the
/// transaction's dependents can still match their recorded edges against it.
#[test]
fn dependency_validated_keeps_its_revision_when_superseded() {
    let mut graph = with_leaf();
    let v2 = graph.head();
    let res1 = value(1);
    let first = compute(&mut graph, key(1), v2, res1.dupe());
    let v3 = inject(&mut graph, key(0), 200);
    let candidate = candidate_at(&graph, key(1), v3);
    let second = compute(&mut graph, key(1), v3, value(2));
    assert_ne!(first.revision(), second.revision());
    // A validation of the first certificate at version 2 that raced the second compute.
    let returned = revalidate(&mut graph, key(1), v2, candidate);
    assert_eq!(returned.revision(), first.revision());
    assert!(returned.testing_resident_value().instance_equal(&res1));
    assert_eq!(
        graph
            .get(VersionedGraphKey::new(v3, key(1)))
            .unpack_match()
            .unwrap()
            .0
            .revision(),
        second.revision()
    );
    // The key is attached to the newer value, so the racing revalidation installs nothing and
    // version 2 is left to re-establish the older certificate on demand.
    assert!(
        graph
            .get(VersionedGraphKey::new(v2, key(1)))
            .unpack_unknown()
            .is_some()
    );
}

/// A revalidation whose value is still paged out keeps it paged out.
#[test]
fn dependency_validated_paged_out_value_stays_paged_out() {
    let mut graph = with_leaf();
    let v2 = graph.head();
    compute(&mut graph, key(1), v2, value(1));
    page_out(&mut graph, key(1), 0x77);
    inject(&mut graph, key(0), 200);
    let v4 = inject(&mut graph, key(0), 100);
    let candidate = candidate_at(&graph, key(1), v4);
    assert!(matches!(candidate.entry, MaybeResident::PagedOut(_)));
    let returned = revalidate(&mut graph, key(1), v4, candidate);
    assert!(returned.paged_out_data_key().is_some());
    assert_eq!(
        graph.pagable_node_counts(),
        PagableNodeCounts {
            resident: 0,
            paged_out: 1,
            candidates: 0
        }
    );
}

/// After a dirty, the stored certificate is offered but cannot be revalidated: its ε is not the
/// version's.
#[test]
fn a_dirtied_key_offers_a_stale_candidate() {
    let mut graph = with_leaf();
    let v2 = graph.head();
    compute(&mut graph, key(1), v2, value(1));
    let v3 = dirty(&mut graph, key(1));
    match graph.get(VersionedGraphKey::new(v3, key(1))) {
        VersionedGraphResult::Unknown {
            candidate: Some(candidate),
            epsilon,
        } => assert_ne!(candidate.cert.epsilon, epsilon),
        other => panic!("expected a stale candidate, got {other:?}"),
    }
    // Recomputing to the same value under the new ε re-finds the revision and re-attaches.
    let before = candidate_at(&graph, key(1), v3).cert.revision;
    let after = compute(&mut graph, key(1), v3, value(1));
    assert_eq!(after.revision(), Some(before));
    assert!(
        graph
            .get(VersionedGraphKey::new(v3, key(1)))
            .unpack_match()
            .is_some()
    );
}

fn counts(resident: usize, paged_out: usize, candidates: usize) -> PagableNodeCounts {
    PagableNodeCounts {
        resident,
        paged_out,
        candidates,
    }
}

/// A computed key whose values are asserted (`changed_to` on a `Key`, which buck2 does for its
/// pageable starlark roots) pages like one that was computed, and keeps its values across `take`.
#[test]
fn asserted_values_of_computed_keys_page_out() {
    let mut graph = VersionedGraph::new();
    let v2 = graph.commit([(
        key(1),
        ChangeType::UpdateValue(value(1), StorageType::Normal),
        InvalidationSourcePriority::Normal,
    )]);
    graph.assert_consistent();
    assert_eq!(graph.pagable_node_counts(), counts(1, 0, 1));
    page_out(&mut graph, key(1), 7);
    assert_eq!(graph.pagable_node_counts(), counts(0, 1, 0));
    assert_eq!(
        graph
            .get(VersionedGraphKey::new(v2, key(1)))
            .unpack_match()
            .unwrap()
            .0
            .paged_out_data_key(),
        Some(pagable::DataKey::testing_new(7))
    );
    graph.rehydrate(key(1), pagable::DataKey::testing_new(7), value(1));
    graph.assert_consistent();
    assert_eq!(graph.pagable_node_counts(), counts(1, 0, 0));
    let v3 = graph.take();
    graph.assert_consistent();
    assert_eq!(graph.pagable_node_counts(), counts(1, 0, 0));
    assert!(
        graph
            .get(VersionedGraphKey::new(v3, key(1)))
            .unpack_match()
            .is_some()
    );
}

#[test]
fn page_out_index_tracks_the_values() {
    let mut graph = with_leaf();
    let v2 = graph.head();
    // An `InjectedKey` takes no part in paging.
    assert_eq!(graph.pagable_node_counts(), counts(0, 0, 0));
    compute(&mut graph, key(1), v2, value(1));
    compute(&mut graph, key(2), v2, value(2));
    assert_eq!(graph.pagable_node_counts(), counts(2, 0, 2));
    page_out(&mut graph, key(1), 1);
    assert_eq!(graph.pagable_node_counts(), counts(1, 1, 1));
    assert_eq!(graph.paged_out_keys().len(), 1);
    graph.rehydrate(key(1), pagable::DataKey::testing_new(1), value(1));
    graph.assert_consistent();
    assert_eq!(graph.pagable_node_counts(), counts(2, 0, 1));
    // A stale page-in for a data key no value holds is ignored.
    graph.rehydrate(key(1), pagable::DataKey::testing_new(99), value(1));
    graph.assert_consistent();
    let k2_result = graph.get(VersionedGraphKey::new(v2, key(2)));
    let (k2_value, _) = k2_result.unpack_match().unwrap();
    let k2_value = k2_value.testing_resident_value().testing_value().dupe();
    graph.mark_non_pageable(vec![(key(2), DiceValidValue::from_arc(k2_value))]);
    graph.assert_consistent();
    assert_eq!(graph.pagable_node_counts(), counts(2, 0, 0));
    assert!(graph.keys_to_page_out().is_empty());
    let v3 = graph.take();
    graph.assert_consistent();
    assert_eq!(graph.pagable_node_counts(), counts(0, 0, 0));
    assert!(
        graph
            .get(VersionedGraphKey::new(v3, key(0)))
            .unpack_match()
            .is_some()
    );
    assert!(
        graph
            .get(VersionedGraphKey::new(v3, key(1)))
            .unpack_unknown()
            .is_some()
    );
}

/// `take` keeps the per-key revision counters, so a value computed after it can never collide
/// with a revision handed out before it.
#[test]
fn take_keeps_revision_counters() {
    let mut graph = with_leaf();
    let v2 = graph.head();
    let before = compute(&mut graph, key(1), v2, value(1));
    let v3 = graph.take();
    graph.assert_consistent();
    let after = compute(&mut graph, key(1), v3, value(2));
    assert!(after.revision().unwrap().as_u32() > before.revision().unwrap().as_u32());
}
