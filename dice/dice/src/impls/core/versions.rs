/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;

use crate::impls::cache::SharedCache;
use crate::versions::VersionNumber;
use crate::HashMap;

/// Tracks the currently in-flight versions for updates and reads to ensure
/// values are up to date.
#[derive(Allocative)]
pub(crate) struct VersionTracker {
    current: VersionNumber,
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

#[derive(Copy, Clone, Eq, Debug, Display, Dupe, PartialEq, Allocative)]
#[display(fmt = "v{}", "_0")]
pub(crate) struct VersionEpoch(usize);

impl VersionEpoch {
    #[cfg(test)]
    pub(crate) fn testing_new(e: usize) -> Self {
        Self(e)
    }
}

#[derive(Derivative, Allocative)]
#[derivative(Debug)]
struct ActiveVersionData {
    #[derivative(Debug = "ignore")]
    per_transaction_data: SharedCache,
    ref_count: usize,
    version_epoch: VersionEpoch,
}

impl VersionTracker {
    pub(crate) fn new() -> Self {
        VersionTracker {
            current: VersionNumber::ZERO,
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

            debug!(
                msg = "Creating new shared state",
                v = %v,
                v_epoch = %version_epoch
            );

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

    /// check if the given version and epoch are "relevant", that is the current active version's
    /// epoch matches the given epoch
    pub(crate) fn is_relevant(&self, v: VersionNumber, epoch: VersionEpoch) -> bool {
        self.active_versions
            .get(&v)
            .map_or(false, |active| active.version_epoch == epoch)
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
            let removed = self.active_versions.remove(&v).expect("existed above");

            debug!(
                msg = "shared state removed",
                v = %v,
                v_epoch = %removed.version_epoch
            );
            Some(removed.per_transaction_data)
        } else {
            None
        }
    }

    /// Requests the 'WriteVersion' that is intended to be used for updates to
    /// the incremental computations
    pub(crate) fn write(&mut self) -> VersionForWrites {
        VersionForWrites { tracker: self }
    }
}

pub(crate) struct VersionForWrites<'a> {
    tracker: &'a mut VersionTracker,
}

impl<'a> VersionForWrites<'a> {
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
    use dupe::Dupe;

    use crate::impls::cache::SharedCache;
    use crate::impls::core::versions::VersionTracker;
    use crate::impls::key::DiceKey;
    use crate::introspection::graph::AnyKey;
    use crate::introspection::graph::VersionNumber;
    use crate::legacy::dice_futures::dice_task::DiceTaskStateForDebugging;
    use crate::HashMap;

    pub(crate) struct VersionIntrospectable(Vec<(usize, SharedCache)>);

    impl VersionIntrospectable {
        pub(crate) fn versions_currently_running(&self) -> Vec<VersionNumber> {
            self.0.iter().map(|(v, _)| VersionNumber(*v)).collect()
        }

        pub(crate) fn keys_currently_running(
            &self,
            key_map: &HashMap<DiceKey, AnyKey>,
        ) -> Vec<(AnyKey, VersionNumber, DiceTaskStateForDebugging)> {
            self.0
                .iter()
                .flat_map(|(v, cache)| {
                    cache.iter_tasks().map(|(k, state)| {
                        (
                            key_map.get(&k).expect("key should exist").clone(),
                            VersionNumber(*v),
                            state,
                        )
                    })
                })
                .collect()
        }

        pub(crate) fn currently_running_key_count(&self) -> usize {
            self.0
                .iter()
                .flat_map(|(_, cache)| {
                    cache.iter_tasks().filter(|(_, state)| match state {
                        DiceTaskStateForDebugging::AsyncInProgress => true,
                        DiceTaskStateForDebugging::SyncInProgress => true,
                        _ => false,
                    })
                })
                .count()
        }
    }

    impl VersionTracker {
        pub(crate) fn introspect(&self) -> VersionIntrospectable {
            VersionIntrospectable(
                self.currently_active()
                    .map(|(v, cache)| (v, cache.dupe()))
                    .collect(),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

    use crate::impls::core::versions::VersionEpoch;
    use crate::impls::core::versions::VersionTracker;
    use crate::versions::VersionNumber;

    #[test]
    fn simple_version_increases() {
        let mut vt = VersionTracker::new();

        let _vg = vt.at(VersionNumber::new(0));
        assert_matches!(
            vt.active_versions.get(&VersionNumber::new(0)), Some(active) if active.ref_count == 1
        );

        let _vg = vt.at(VersionNumber::new(0));
        assert_matches!(
            vt.active_versions.get(&VersionNumber::new(0)), Some(active) if active.ref_count == 2
        );

        vt.drop_at_version(VersionNumber::new(0));
        assert_matches!(
            vt.active_versions.get(&VersionNumber::new(0)), Some(active) if active.ref_count == 1
        );

        vt.drop_at_version(VersionNumber::new(0));
        assert_matches!(vt.active_versions.get(&VersionNumber::new(0)), None);
    }

    #[test]
    fn version_epoch_relevant() {
        let mut vt = VersionTracker::new();

        let (epoch, _s) = vt.at(VersionNumber::new(0));
        assert!(vt.is_relevant(VersionNumber::new(0), epoch));
        assert!(!vt.is_relevant(VersionNumber::new(0), VersionEpoch::testing_new(9999)));

        let (epoch1, _s) = vt.at(VersionNumber::new(2));
        assert!(!vt.is_relevant(VersionNumber::new(0), epoch1));
        assert!(vt.is_relevant(VersionNumber::new(2), epoch1));
        assert!(!vt.is_relevant(VersionNumber::new(2), epoch));

        vt.drop_at_version(VersionNumber::new(2));
        let (epoch2, _s) = vt.at(VersionNumber::new(2));
        assert!(!vt.is_relevant(VersionNumber::new(0), epoch1));
        assert!(!vt.is_relevant(VersionNumber::new(2), epoch1));
        assert!(vt.is_relevant(VersionNumber::new(2), epoch2));
    }

    #[test]
    fn write_version_commits_and_undo() {
        let mut vt = VersionTracker::new();

        {
            let v1 = vt.write();
            assert_eq!(v1.version(), VersionNumber::new(1));
            assert_eq!(v1.version(), VersionNumber::new(1));

            assert_eq!(v1.commit(), VersionNumber::new(1));
            assert_eq!(vt.current(), VersionNumber::new(1));
        }

        {
            let v2 = vt.write();
            assert_eq!(v2.version(), VersionNumber::new(2));

            assert_eq!(v2.undo(), VersionNumber::new(1));
            assert_eq!(vt.current(), VersionNumber::new(1));
        }
    }
}
