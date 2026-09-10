/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The value layer of the core state (`docs/incrementality.md` Appendix A) over `dice_core`:
//! the values behind revisions, their interning, retention and page-out lifecycle.

use allocative::Allocative;
use bit_set::BitSet;
use dice_core::Cert;
use dice_core::Change;
use dice_core::CoreState;
use dice_core::Env;
use dice_core::KeyMap;
use dice_core::Lookup;
use dice_core::ValidSource;
use dupe::Dupe;
use pagable::DataKey;
use smallvec::SmallVec;

use crate::api::key::InvalidationSourcePriority;
use crate::api::storage_type::StorageType;
use crate::arc::Arc;
use crate::core::graph::lifecycle::PagableValue;
use crate::core::graph::revision::EpsilonToken;
use crate::core::graph::revision::Revision;
use crate::core::graph::revision::RevisionMint;
use crate::core::graph::types::Candidate;
use crate::core::graph::types::VersionedGraphKey;
use crate::core::graph::types::VersionedGraphResult;
use crate::deps::graph::SeriesParallelDeps;
use crate::dice::PagableNodeCounts;
use crate::key::DiceKey;
use crate::updater::ChangeType;
use crate::value::DiceComputedValue;
use crate::value::DiceValidValue;
use crate::value::MaybeResident;
use crate::value::PageOutResult;
use crate::value::TrackedInvalidationPaths;
use crate::versions::VersionNumber;

pub(crate) mod introspection;
pub(crate) mod lifecycle;
pub(crate) mod revision;
#[cfg(test)]
mod tests;
pub(crate) mod types;

/// Dice's environment for the core state: the deps of a certificate keep their series-parallel
/// shape, a claim carries the invalidation paths of its compute, and every assertion its
/// invalidation source priority.
#[derive(Allocative)]
pub(crate) struct DiceEnv;

impl Env for DiceEnv {
    type Premises = SeriesParallelDeps;
    type ClaimData = TrackedInvalidationPaths;
    type AssertionData = InvalidationSourcePriority;
}

pub(crate) type DiceCert = Cert<SeriesParallelDeps>;

mini_vec::size_assert::words_of_type!(DiceCert, 4);
mini_vec::size_assert::words_of_type!(dice_core::Claim<DiceEnv>, 3);
mini_vec::size_assert::words_of_type!(dice_core::Slot<DiceEnv>, 5);
mini_vec::size_assert::words_of_type!(KeyValues, 6);
mini_vec::size_assert::words_of_type!(Candidate, 5);

/// A write, as the worker reports it.
pub(crate) enum ValueUpdate {
    /// A value produced by running the key's computation: Appendix A's fused write.
    Computed {
        value: DiceValidValue,
        deps: SeriesParallelDeps,
        epsilon: EpsilonToken,
    },
    /// A certificate handed out at lookup whose premises the worker has re-established, re-issued
    /// as is. The value it carries is the one handed out, which may still be paged out.
    DependencyValidated { candidate: Candidate },
}

/// The values retained for one key.
#[derive(Allocative, Default)]
struct KeyValues {
    mint: RevisionMint,
    /// Whether the key's values are asserted rather than computed. Every value of an asserted key
    /// is retained, since none can be recomputed and the key's assertion history names them all.
    asserted: bool,
    /// Whether the key is an `InjectedKey`, which takes no part in paging. A computed key whose
    /// values are asserted (buck2 does this for its pageable starlark roots) pages like any other.
    injected: bool,
    entries: SmallVec<[(Revision, PagableValue); 1]>,
}

impl KeyValues {
    fn entry(&self, revision: Revision) -> Option<&PagableValue> {
        self.entries
            .iter()
            .find_map(|(r, v)| (*r == revision).then_some(v))
    }

    fn entry_mut(&mut self, revision: Revision) -> Option<&mut PagableValue> {
        self.entries
            .iter_mut()
            .find_map(|(r, v)| (*r == revision).then_some(v))
    }

    fn lifecycle(&self) -> fn(DiceValidValue) -> PagableValue {
        PagableValue::lifecycle_after(self.entries.last().map(|(_, v)| v))
    }

    fn insert(&mut self, revision: Revision, value: PagableValue) {
        debug_assert!(self.entry(revision).is_none());
        self.entries.push((revision, value));
    }

    /// Appendix A step 1: the value behind `revision` is the canonical one for the trace, and
    /// `value` is discarded, unless the stored value is paged out and `value` takes its place.
    fn adopt(&mut self, revision: Revision, value: DiceValidValue) -> DiceValidValue {
        match self.entry_mut(revision) {
            Some(stored) => {
                stored.make_resident(value);
                stored.as_hydrated().expect("made resident above").dupe()
            }
            None => {
                let lifecycle = self.lifecycle();
                self.insert(revision, lifecycle(value.dupe()));
                value
            }
        }
    }

    /// Appendix A steps 2 and 3. Returns the revision and the canonical value behind it.
    fn intern_computed(&mut self, value: DiceValidValue) -> (Revision, DiceValidValue) {
        for (revision, stored) in &self.entries {
            if let Some(existing) = stored.as_hydrated()
                && existing.equality(&value)
            {
                return (*revision, existing.dupe());
            }
        }
        let revision = self.mint.mint();
        let lifecycle = self.lifecycle();
        self.insert(revision, lifecycle(value.dupe()));
        (revision, value)
    }

    /// An injected value, compared against every retained value so that a key returned to an
    /// earlier value gets that value's revision back.
    fn intern_asserted(&mut self, value: DiceValidValue, storage: StorageType) -> Revision {
        self.asserted = true;
        self.injected = matches!(storage, StorageType::Injected);
        for (revision, stored) in &self.entries {
            if stored.as_hydrated().is_some_and(|e| e.equality(&value)) {
                return *revision;
            }
        }
        let revision = self.mint.mint();
        let entry = if self.injected {
            PagableValue::NonPageable(value)
        } else {
            (self.lifecycle())(value)
        };
        self.insert(revision, entry);
        revision
    }

    /// The value behind a re-issued certificate: the retained one if there is one, made resident
    /// from `value` if it can be, else `value` itself, retained.
    fn restore(
        &mut self,
        revision: Revision,
        value: MaybeResident<DiceValidValue>,
    ) -> MaybeResident<DiceValidValue> {
        match self.entry_mut(revision) {
            Some(stored) => {
                if let MaybeResident::Resident(value) = value {
                    stored.make_resident(value);
                }
                stored.as_maybe_resident()
            }
            None => {
                let entry = match value {
                    MaybeResident::Resident(value) => (self.lifecycle())(value),
                    MaybeResident::PagedOut(data_key) => PagableValue::PagedOut(data_key),
                };
                let out = entry.as_maybe_resident();
                self.insert(revision, entry);
                out
            }
        }
    }

    fn contribution(&self) -> Contribution {
        let mut c = Contribution::default();
        if self.injected {
            return c;
        }
        for (_, value) in &self.entries {
            match value {
                PagableValue::PagedOut(_) => c.paged_out += 1,
                PagableValue::NeverPagedOut(_) => {
                    c.resident += 1;
                    c.candidates += 1;
                }
                PagableValue::NonPageable(_) | PagableValue::Recomputed(_) => c.resident += 1,
            }
        }
        c
    }
}

/// A key's share of the paging tallies.
#[derive(Default, Copy, Clone, PartialEq, Eq)]
struct Contribution {
    resident: usize,
    paged_out: usize,
    candidates: usize,
}

/// The page-out candidate index and the resident / paged-out tallies, kept in step with the
/// values by [`VersionedGraph::with_values`] so that no operation needs a scan.
#[derive(Default)]
struct PagableIndex {
    /// Keys with at least one page-out candidate, by `DiceKey::index`.
    candidates: BitSet,
    candidate_count: usize,
    resident_count: usize,
    paged_out_count: usize,
}

impl PagableIndex {
    fn apply(&mut self, key: DiceKey, before: Contribution, after: Contribution) {
        if before == after {
            return;
        }
        self.resident_count = self.resident_count + after.resident - before.resident;
        self.paged_out_count = self.paged_out_count + after.paged_out - before.paged_out;
        self.candidate_count = self.candidate_count + after.candidates - before.candidates;
        match (before.candidates > 0, after.candidates > 0) {
            (false, true) => {
                self.candidates.insert(key.index as usize);
            }
            (true, false) => {
                self.candidates.remove(key.index as usize);
            }
            _ => {}
        }
    }
}

/// The core state with its values: what the actor thread operates on.
#[derive(Allocative)]
pub(crate) struct VersionedGraph {
    core: CoreState<DiceEnv>,
    values: KeyMap<KeyValues>,
    #[allocative(skip)]
    index: PagableIndex,
}

impl VersionedGraph {
    pub(crate) fn new() -> Self {
        VersionedGraph {
            core: CoreState::new(),
            values: KeyMap::default(),
            index: PagableIndex::default(),
        }
    }

    pub(crate) fn core(&self) -> &CoreState<DiceEnv> {
        &self.core
    }

    /// The newest version.
    pub(crate) fn head(&self) -> VersionNumber {
        self.core.head(self.core.root())
    }

    /// The one path through which a key's values change, so that the paging index follows.
    fn with_values<R>(&mut self, key: DiceKey, f: impl FnOnce(&mut KeyValues) -> R) -> R {
        Self::with_values_of(&mut self.values, &mut self.index, key, f)
    }

    fn with_values_of<R>(
        values: &mut KeyMap<KeyValues>,
        index: &mut PagableIndex,
        key: DiceKey,
        f: impl FnOnce(&mut KeyValues) -> R,
    ) -> R {
        let key_values = values.entry(key).or_default();
        let before = key_values.contribution();
        let result = f(key_values);
        let after = key_values.contribution();
        index.apply(key, before, after);
        result
    }

    /// The value named by `revision`, which some claim of `key` names and is therefore retained.
    fn stored(&self, key: DiceKey, revision: Revision) -> MaybeResident<DiceValidValue> {
        self.values
            .get(&key)
            .and_then(|kv| kv.entry(revision))
            .map(|v| v.as_maybe_resident())
            .unwrap_or_else(|| panic!("no value retained for {key:?} at {revision:?}"))
    }

    /// Commits the changes to the newest version and returns the version they made, which is the
    /// same one if none of them had an effect.
    pub(crate) fn commit(
        &mut self,
        changes: impl IntoIterator<Item = (DiceKey, ChangeType, InvalidationSourcePriority)>,
    ) -> VersionNumber {
        let mut core_changes = Vec::new();
        for (key, change, priority) in changes {
            match change {
                ChangeType::Invalidate => core_changes.push(Change::Dirty {
                    key,
                    data: priority,
                }),
                ChangeType::UpdateValue(value, storage) => {
                    let revision = self.with_values(key, |kv| kv.intern_asserted(value, storage));
                    core_changes.push(Change::Assert {
                        key,
                        revision,
                        data: priority,
                    });
                }
            }
        }
        let root = self.core.root();
        self.core.commit(root, core_changes)
    }

    /// Forgets every claim and every computed value, keeping injected keys and per-key revision
    /// counters, and returns the fresh version at which nothing resolves.
    pub(crate) fn take(&mut self) -> VersionNumber {
        let version = self.core.take();
        let keys: Vec<DiceKey> = self.values.keys().copied().collect();
        let mut dropped = Vec::new();
        for key in keys {
            let taken = self.with_values(key, |kv| {
                if kv.asserted {
                    SmallVec::new()
                } else {
                    std::mem::take(&mut kv.entries)
                }
            });
            dropped.push(taken);
        }
        // There may be a lot to drop; do it off the actor thread.
        std::thread::Builder::new()
            .name("dice-drop-everything".to_owned())
            .spawn(move || drop(dropped))
            .expect("failed to spawn thread");
        version
    }

    pub(crate) fn get(&self, at: VersionedGraphKey) -> VersionedGraphResult {
        let (key, v) = (at.k, at.v);
        match self.core.lookup(key, v) {
            Lookup::Valid { revision, source } => {
                let invalidation_paths = match &source {
                    ValidSource::Claim { data, dirtied, .. } => {
                        let mut paths = data.at_version(v);
                        if let Some(dirtied) = dirtied {
                            paths.update(&TrackedInvalidationPaths::new(
                                dirtied.data,
                                key,
                                dirtied.version,
                            ));
                        }
                        paths
                    }
                    ValidSource::Asserted { assertion } => {
                        TrackedInvalidationPaths::new(assertion.data, key, assertion.version)
                    }
                };
                VersionedGraphResult::Match {
                    value: DiceComputedValue::new(
                        self.stored(key, revision).into_payload(),
                        invalidation_paths,
                        revision,
                    ),
                    epsilon: self.core.epsilon(key, v),
                }
            }
            Lookup::Unknown { candidate, epsilon } => {
                let (cert, revalidatable) = match candidate {
                    Some(cert) => (Some(cert), true),
                    None => (self.core.nearest_certificate(key, v), false),
                };
                VersionedGraphResult::Unknown {
                    candidate: cert.map(|cert| Candidate {
                        entry: self.stored(key, cert.revision),
                        cert: cert.dupe(),
                        revalidatable,
                    }),
                    epsilon,
                }
            }
        }
    }

    /// Writes a value computed, or a certificate re-established, by a transaction at `at.v`.
    /// Returns the value the transaction should use: the canonical instance for the revision.
    pub(crate) fn update(
        &mut self,
        at: VersionedGraphKey,
        update: ValueUpdate,
        invalidation_paths: TrackedInvalidationPaths,
    ) -> DiceComputedValue {
        let key = at.k;
        let invalidation_paths = invalidation_paths.for_dependent(key);
        let (cert, value): (Arc<DiceCert>, MaybeResident<DiceValidValue>) = match update {
            ValueUpdate::Computed {
                value,
                mut deps,
                epsilon,
            } => {
                debug_assert_eq!(
                    epsilon,
                    self.core.epsilon(key, at.v),
                    "a write's ε must be the one its transaction looked up"
                );
                let identical = self
                    .core
                    .pinned_certs(key)
                    .find(|c| c.epsilon == epsilon && c.deps == deps)
                    .cloned();
                match identical {
                    Some(cert) => {
                        let stored = self.with_values(key, |kv| kv.adopt(cert.revision, value));
                        (cert, MaybeResident::Resident(stored))
                    }
                    None => {
                        let (revision, stored) =
                            self.with_values(key, |kv| kv.intern_computed(value));
                        deps.shrink_to_fit();
                        (
                            Arc::new(Cert::new(key, revision, deps, epsilon)),
                            MaybeResident::Resident(stored),
                        )
                    }
                }
            }
            ValueUpdate::DependencyValidated { candidate } => {
                let revision = candidate.cert.revision;
                let value = self.with_values(key, |kv| kv.restore(revision, candidate.entry));
                (candidate.cert, value)
            }
        };
        let revision = cert.revision;
        self.core.write(cert, invalidation_paths.dupe());
        let core = &self.core;
        Self::with_values_of(&mut self.values, &mut self.index, key, |kv| {
            kv.entries.retain(|(r, _)| core.is_referenced(key, *r));
        });
        let mut paths = invalidation_paths;
        if let Some(dirtied) = self.core.dirtied_at(key, at.v) {
            paths.update(&TrackedInvalidationPaths::new(
                dirtied.data,
                key,
                dirtied.version,
            ));
        }
        DiceComputedValue::new(value.into_payload(), paths, revision)
    }

    /// The number of keys the state holds anything for.
    pub(crate) fn key_count(&self) -> usize {
        self.core.key_count()
    }

    /// Every resident value that has never been paged out, with its key.
    pub(crate) fn keys_to_page_out(&self) -> Vec<(DiceKey, DiceValidValue)> {
        let mut out = Vec::with_capacity(self.index.candidate_count);
        for index in self.index.candidates.iter() {
            let key = DiceKey {
                index: index as u32,
            };
            if let Some(kv) = self.values.get(&key) {
                for (_, value) in &kv.entries {
                    if value.is_page_out_candidate() {
                        out.push((
                            key,
                            value.as_hydrated().expect("a candidate is resident").dupe(),
                        ));
                    }
                }
            }
        }
        out
    }

    /// Every paged-out value, with its key.
    pub(crate) fn paged_out_keys(&self) -> Vec<(DiceKey, DataKey)> {
        let mut out = Vec::with_capacity(self.index.paged_out_count);
        for (key, kv) in &self.values {
            if kv.injected {
                continue;
            }
            for (_, value) in &kv.entries {
                if let Some(data_key) = value.data_key() {
                    out.push((*key, data_key));
                }
            }
        }
        out
    }

    /// The keys of every resident and every paged-out value that takes part in paging, a key
    /// once per value.
    pub(crate) fn resident_and_paged_out(&self) -> (Vec<DiceKey>, Vec<DiceKey>) {
        let mut resident = Vec::with_capacity(self.index.resident_count);
        let mut paged_out = Vec::with_capacity(self.index.paged_out_count);
        for (key, kv) in &self.values {
            if kv.injected {
                continue;
            }
            for (_, value) in &kv.entries {
                if value.as_hydrated().is_some() {
                    resident.push(*key);
                } else {
                    paged_out.push(*key);
                }
            }
        }
        (resident, paged_out)
    }

    pub(crate) fn pagable_node_counts(&self) -> PagableNodeCounts {
        PagableNodeCounts {
            resident: self.index.resident_count,
            paged_out: self.index.paged_out_count,
            candidates: self.index.candidate_count,
        }
    }

    /// Evicts each value that is still the exact allocation page-out serialized. A value that a
    /// write replaced in the meantime is left alone.
    pub(crate) fn evict_keys(&mut self, keys: Vec<(DiceKey, PageOutResult)>) {
        for (
            key,
            PageOutResult {
                serialized_value,
                data_key,
            },
        ) in keys
        {
            self.with_values(key, |kv| {
                for (_, value) in kv.entries.iter_mut() {
                    if value
                        .as_hydrated()
                        .is_some_and(|current| current.ptr_eq(&serialized_value))
                    {
                        *value = PagableValue::PagedOut(data_key);
                    }
                }
            });
        }
    }

    /// Marks each value page-out could not serialize, if it is still the exact allocation
    /// inspected, so that it is not offered as a candidate again.
    pub(crate) fn mark_non_pageable(&mut self, keys: Vec<(DiceKey, DiceValidValue)>) {
        for (key, inspected) in keys {
            self.with_values(key, |kv| {
                for (_, value) in kv.entries.iter_mut() {
                    if value
                        .as_hydrated()
                        .is_some_and(|current| current.ptr_eq(&inspected))
                    {
                        *value = PagableValue::NonPageable(inspected.dupe());
                    }
                }
            });
        }
    }

    /// Makes the value paged out at `data_key` resident again. No-op if no such value is
    /// retained any more.
    pub(crate) fn rehydrate(&mut self, key: DiceKey, data_key: DataKey, value: DiceValidValue) {
        self.with_values(key, |kv| {
            if let Some((_, stored)) = kv
                .entries
                .iter_mut()
                .find(|(_, v)| v.data_key() == Some(data_key))
            {
                stored.make_resident(value);
            }
        });
    }

    /// Asserts, by full scan, that the paging index matches the values, that the core's own
    /// invariants hold, and that values and claims reference each other exactly: every
    /// certificate a claim pins has its value retained, and every computed value retained is
    /// named by some claim.
    #[cfg(test)]
    pub(crate) fn assert_consistent(&self) {
        self.core.check_invariants();
        let mut total = Contribution::default();
        let mut candidates = Vec::new();
        for (key, kv) in &self.values {
            let c = kv.contribution();
            total.resident += c.resident;
            total.paged_out += c.paged_out;
            total.candidates += c.candidates;
            if c.candidates > 0 {
                candidates.push(key.index as usize);
            }
            for cert in self.core.pinned_certs(*key) {
                assert!(
                    kv.entry(cert.revision).is_some(),
                    "{key:?} pins a certificate at {:?} but retains no value for it",
                    cert.revision
                );
            }
            if !kv.asserted {
                for (revision, _) in &kv.entries {
                    assert!(
                        self.core.is_referenced(*key, *revision),
                        "{key:?} retains {revision:?}, which no claim names"
                    );
                }
            }
        }
        candidates.sort_unstable();
        assert_eq!(self.index.candidates.iter().collect::<Vec<_>>(), candidates);
        assert_eq!(self.index.candidate_count, total.candidates);
        assert_eq!(self.index.resident_count, total.resident);
        assert_eq!(self.index.paged_out_count, total.paged_out);
    }
}
