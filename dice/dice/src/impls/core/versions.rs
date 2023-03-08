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
}

#[derive(Derivative, Allocative)]
#[derivative(Debug)]
struct ActiveVersionData {
    #[derivative(Debug = "ignore")]
    per_transaction_data: SharedCache,
    ref_count: usize,
}

impl VersionTracker {
    pub(crate) fn new() -> Self {
        VersionTracker {
            current: VersionNumber::ZERO,
            active_versions: HashMap::default(),
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

    pub(crate) fn at(&mut self, v: VersionNumber) -> SharedCache {
        let mut entry = self
            .active_versions
            .entry(v)
            .or_insert_with(|| ActiveVersionData {
                // TODO properly create the PerLiveTransactionCtx
                per_transaction_data: SharedCache::new(),
                ref_count: 0,
            });

        entry.ref_count += 1;

        entry.per_transaction_data.dupe()
    }

    /// Drops reference to a VersionNumber given the token
    pub(crate) fn drop_at_version(&mut self, v: VersionNumber) {
        let ref_count = {
            let entry = self
                .active_versions
                .get_mut(&v)
                .expect("shouldn't be able to return version without obtaining one");

            entry.ref_count -= 1;
            entry.ref_count
        };

        if ref_count == 0 {
            self.active_versions.remove(&v).expect("existed above");
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

    #[allow(unused)]
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

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

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
