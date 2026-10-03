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
use dupe::Dupe;

use crate::HashMap;
use crate::epoch::cache::SharedCache;
use crate::versions::VersionNumber;

/// The transactions in flight: one shared task cache per active version.
#[derive(Allocative)]
pub(crate) struct VersionTracker {
    /// Tracks the currently active versions and how many contexts are holding each of them.
    active_versions: HashMap<VersionNumber, ActiveVersionData>,
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

    /// Drops reference to a VersionNumber given the token
    pub(crate) fn drop_at_version(&mut self, v: VersionNumber) -> Option<SharedCache> {
        let ref_count = {
            let entry = self
                .active_versions
                .get_mut(&v)
                .expect("shouldn't be able to return version without obtaining one");

            entry.ref_count -= 1;
            entry.ref_count
        };

        if ref_count == 0 {
            Some(
                self.active_versions
                    .remove(&v)
                    .expect("existed above")
                    .per_transaction_data,
            )
        } else {
            None
        }
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
