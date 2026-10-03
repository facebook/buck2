/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use dice_core::BranchId;
use dupe::Dupe;

use crate::HashMap;
use crate::epoch::cache::SharedCache;
use crate::epoch::cache::WeakSharedCache;
use crate::epoch::task::dice::DiceTask;
use crate::versions::VersionNumber;

/// The transactions in flight: one shared task cache per active version, and the caches of the
/// versions whose transactions are all gone while a task of theirs may still be running.
#[derive(Allocative)]
pub(crate) struct VersionTracker {
    /// Tracks the currently active versions and how many contexts are holding each of them.
    active_versions: HashMap<VersionNumber, ActiveVersionData>,
    /// The caches of versions no transaction holds any more. Held weakly: the workers still
    /// running in a cache keep it alive, and it goes away with the last of them.
    #[allocative(skip)]
    draining: Vec<(VersionNumber, WeakSharedCache)>,
}

#[derive(Debug, Allocative)]
struct ActiveVersionData {
    per_transaction_data: SharedCache,
    ref_count: usize,
}

impl VersionTracker {
    pub(crate) fn new() -> Self {
        VersionTracker {
            active_versions: HashMap::default(),
            draining: Vec::new(),
        }
    }

    pub(crate) fn currently_active(&self) -> impl Iterator<Item = (usize, &SharedCache)> {
        self.active_versions
            .values()
            .map(|data| (data.ref_count, &data.per_transaction_data))
    }

    pub(crate) fn at(&mut self, v: VersionNumber) -> SharedCache {
        let entry = self
            .active_versions
            .entry(v)
            .or_insert_with(|| ActiveVersionData {
                per_transaction_data: SharedCache::new(),
                ref_count: 0,
            });

        entry.ref_count += 1;

        entry.per_transaction_data.dupe()
    }

    /// Drops one reference to `v`. The last one dropped leaves the version's cache to drain:
    /// the tasks in it go on as they were, but no later transaction shares them.
    pub(crate) fn drop_at_version(&mut self, v: VersionNumber) {
        let entry = self
            .active_versions
            .get_mut(&v)
            .expect("shouldn't be able to return version without obtaining one");

        entry.ref_count -= 1;
        if entry.ref_count == 0 {
            let data = self.active_versions.remove(&v).expect("existed above");
            self.draining.retain(|(_, cache)| cache.is_alive());
            self.draining
                .push((v, data.per_transaction_data.downgrade()));
        }
    }

    /// Every task that may still be running, whether its transaction is alive or gone, at the
    /// versions of `branch`, or at every version with `None`.
    ///
    /// This scans every task of every cache in scope, so it costs time proportional to the work
    /// in flight; callers ask at command boundaries, not on hot paths.
    pub(crate) fn pending_tasks(&mut self, branch: Option<BranchId>) -> Vec<DiceTask> {
        let in_scope = |v: &VersionNumber| branch.is_none_or(|b| v.branch() == b);
        let mut pending = Vec::new();
        for (v, active) in &self.active_versions {
            if in_scope(v) {
                pending.extend(active.per_transaction_data.pending_tasks());
            }
        }
        self.draining.retain(|(v, weak)| {
            let Some(cache) = weak.upgrade() else {
                return false;
            };
            if !in_scope(v) {
                return true;
            }
            let tasks = cache.pending_tasks();
            let keep = !tasks.is_empty();
            pending.extend(tasks);
            keep
        });
        pending
    }
}

pub(crate) mod introspection {

    use crate::HashMap;
    use crate::core::versions::VersionTracker;
    use crate::introspection::DiceTaskState;
    use crate::introspection::graph::AnyKey;
    use crate::introspection::graph::VersionNumber;
    use crate::key::DiceKey;

    pub(crate) struct VersionIntrospectable(Vec<(usize, HashMap<DiceKey, DiceTaskState>)>);

    impl VersionIntrospectable {
        #[allow(dead_code)]
        pub(crate) fn versions_currently_running(&self) -> Vec<VersionNumber> {
            self.0.iter().map(|(v, _)| VersionNumber(*v)).collect()
        }

        pub(crate) fn keys_currently_running(
            &self,
            key_map: &HashMap<DiceKey, AnyKey>,
        ) -> Vec<(AnyKey, VersionNumber, DiceTaskState)> {
            self.0
                .iter()
                .flat_map(|(v, cache)| {
                    cache.iter().map(|(k, state)| {
                        (
                            key_map.get(k).expect("key should exist").clone(),
                            VersionNumber(*v),
                            *state,
                        )
                    })
                })
                .collect()
        }
    }

    impl VersionTracker {
        pub(crate) fn introspect(&self) -> VersionIntrospectable {
            // take a snapshot of the currently running graph so that we ensure all the graphs we
            // capture is at one instance in time. This way, we don't end up reading extra keys
            // and having missing key information when we read the graphs later
            VersionIntrospectable(
                self.currently_active()
                    .map(|(v, cache)| (v, cache.iter_tasks().collect()))
                    .collect(),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

    use crate::core::versions::VersionTracker;
    use crate::versions::VersionNumber;

    #[test]
    fn simple_version_increases() {
        let mut vt = VersionTracker::new();

        let _vg = vt.at(VersionNumber::testing_new(1));
        assert_matches!(
            vt.active_versions.get(&VersionNumber::testing_new(1)), Some(active) if active.ref_count == 1
        );

        let _vg = vt.at(VersionNumber::testing_new(1));
        assert_matches!(
            vt.active_versions.get(&VersionNumber::testing_new(1)), Some(active) if active.ref_count == 2
        );

        vt.drop_at_version(VersionNumber::testing_new(1));
        assert_matches!(
            vt.active_versions.get(&VersionNumber::testing_new(1)), Some(active) if active.ref_count == 1
        );

        vt.drop_at_version(VersionNumber::testing_new(1));
        assert_matches!(vt.active_versions.get(&VersionNumber::testing_new(1)), None);
    }
}
