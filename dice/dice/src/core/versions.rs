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

/// Tracks the currently in-flight versions for updates and reads to ensure
/// values are up to date.
#[derive(Allocative)]
pub(crate) struct VersionTracker {
    current: VersionNumber,
    discarded_before: VersionNumber,
    /// Tracks the currently active versions and how many contexts are holding each of them.
    active_versions: HashMap<VersionNumber, ActiveVersionData>,
    epoch_tracker: VersionEpochTracker,
}

// an epoch counter that increases each time we create a new instance of state at a version. This
// will increase the first time we create a version AND when we reuse a version that has been idle
#[derive(Allocative)]
struct VersionEpochTracker(usize);

impl VersionEpochTracker {
    fn next(&mut self) -> VersionEpoch {
        let e = VersionEpoch(self.0);
        self.0 += 1;

        e
    }

    fn new() -> Self {
        Self(0)
    }
}

#[derive(Copy, Clone, Eq, Debug, Dupe, PartialEq, Allocative)]
pub(crate) struct VersionEpoch(usize);

impl VersionEpoch {
    #[cfg(test)]
    pub(crate) fn testing_new(e: usize) -> Self {
        Self(e)
    }
}

#[derive(Debug, Allocative)]
struct ActiveVersionData {
    per_transaction_data: SharedCache,
    ref_count: usize,
    version_epoch: VersionEpoch,
}

impl VersionTracker {
    pub(crate) fn new() -> Self {
        VersionTracker {
            current: VersionNumber::FIRST,
            discarded_before: VersionNumber::FIRST,
            active_versions: HashMap::default(),
            epoch_tracker: VersionEpochTracker::new(),
        }
    }

    pub(crate) fn currently_active(&self) -> impl Iterator<Item = (usize, &SharedCache)> {
        self.active_versions
            .values()
            .map(|data| (data.ref_count, &data.per_transaction_data))
    }

    /// hands out the current "latest" committed version's associated transaction context
    pub(crate) fn current(&self) -> VersionNumber {
        self.current
    }

    pub(crate) fn at(&mut self, v: VersionNumber) -> (VersionEpoch, SharedCache) {
        let entry = self.active_versions.entry(v).or_insert_with(|| {
            let version_epoch = self.epoch_tracker.next();

            ActiveVersionData {
                // TODO properly create the PerLiveTransactionCtx
                per_transaction_data: SharedCache::new(),
                ref_count: 0,
                version_epoch,
            }
        });

        entry.ref_count += 1;

        (entry.version_epoch, entry.per_transaction_data.dupe())
    }

    /// Check whether computation associated with the given major version and epoch has or has not
    /// been cancelled
    pub(crate) fn is_cancelled(&self, v: VersionNumber, epoch: VersionEpoch) -> bool {
        v < self.discarded_before
            || self
                .active_versions
                .get(&v)
                .is_none_or(|active| active.version_epoch != epoch)
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

    /// Requests the 'WriteVersion' that is intended to be used for updates to
    /// the incremental computations
    pub(crate) fn write(&mut self) -> VersionForWrites<'_> {
        VersionForWrites { tracker: self }
    }

    pub(crate) fn clear(&mut self) {
        // FIXME(JakobDegen): What's the safety story supposed to be here? It seems like we've made
        // no attempt to stop there from being an ongoing transaction, and if there is we don't even
        // attempt to actually cancel it. That seems obviously very unsound?
        self.current.inc();
        self.discarded_before = self.current;
    }
}

pub(crate) struct VersionForWrites<'a> {
    tracker: &'a mut VersionTracker,
}

impl VersionForWrites<'_> {
    /// Commits the version write and increases the global version number
    pub(crate) fn commit(self) -> VersionNumber {
        self.tracker.current.inc();
        self.tracker.current
    }

    pub(crate) fn version(&self) -> VersionNumber {
        let mut v = self.tracker.current;
        v.inc();

        v
    }

    /// Undo the pending write to version
    pub(crate) fn undo(self) -> VersionNumber {
        self.tracker.current
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

    use crate::core::versions::VersionEpoch;
    use crate::core::versions::VersionTracker;
    use crate::versions::VersionNumber;

    #[test]
    fn simple_version_increases() {
        let mut vt = VersionTracker::new();

        let _vg = vt.at(VersionNumber::new(1));
        assert_matches!(
            vt.active_versions.get(&VersionNumber::new(1)), Some(active) if active.ref_count == 1
        );

        let _vg = vt.at(VersionNumber::new(1));
        assert_matches!(
            vt.active_versions.get(&VersionNumber::new(1)), Some(active) if active.ref_count == 2
        );

        vt.drop_at_version(VersionNumber::new(1));
        assert_matches!(
            vt.active_versions.get(&VersionNumber::new(1)), Some(active) if active.ref_count == 1
        );

        vt.drop_at_version(VersionNumber::new(1));
        assert_matches!(vt.active_versions.get(&VersionNumber::new(1)), None);
    }

    #[test]
    fn version_epoch_relevant() {
        let mut vt = VersionTracker::new();

        let (epoch, _s) = vt.at(VersionNumber::new(1));
        assert!(!vt.is_cancelled(VersionNumber::new(1), epoch));
        assert!(vt.is_cancelled(VersionNumber::new(1), VersionEpoch::testing_new(9999)));

        let (epoch1, _s) = vt.at(VersionNumber::new(3));
        assert!(vt.is_cancelled(VersionNumber::new(1), epoch1));
        assert!(!vt.is_cancelled(VersionNumber::new(3), epoch1));
        assert!(vt.is_cancelled(VersionNumber::new(3), epoch));

        vt.drop_at_version(VersionNumber::new(3));
        let (epoch2, _s) = vt.at(VersionNumber::new(3));
        assert!(vt.is_cancelled(VersionNumber::new(1), epoch1));
        assert!(vt.is_cancelled(VersionNumber::new(3), epoch1));
        assert!(!vt.is_cancelled(VersionNumber::new(3), epoch2));
    }

    #[test]
    fn write_version_commits_and_undo() {
        let mut vt = VersionTracker::new();

        {
            let v1 = vt.write();
            assert_eq!(v1.version(), VersionNumber::new(2));
            assert_eq!(v1.version(), VersionNumber::new(2));

            assert_eq!(v1.commit(), VersionNumber::new(2));
            assert_eq!(vt.current(), VersionNumber::new(2));
        }

        {
            let v2 = vt.write();
            assert_eq!(v2.version(), VersionNumber::new(3));

            assert_eq!(v2.undo(), VersionNumber::new(2));
            assert_eq!(vt.current(), VersionNumber::new(2));
        }
    }
}
