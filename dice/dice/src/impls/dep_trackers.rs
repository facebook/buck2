/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Trackers that records dependencies and reverse dependencies during execution of requested nodes

use allocative::Allocative;

use crate::impls::key::DiceKey;
use crate::HashSet;

/// The 'DepsTracker' is used to record dependencies of a particular compute node by calling
/// 'record' for each dependency, and then getting a list of 'Dependency's at the end by calling
/// 'collect_deps'.
#[derive(Allocative)]
pub(crate) struct RecordingDepsTracker {
    deps: HashSet<DiceKey>,
}

impl RecordingDepsTracker {
    pub(crate) fn new() -> Self {
        Self {
            deps: HashSet::default(),
        }
    }

    pub(crate) fn record(&mut self, k: DiceKey) {
        self.deps.insert(k);
    }

    pub(crate) fn collect_deps(self) -> HashSet<DiceKey> {
        self.deps
    }
}

#[cfg(test)]
pub(crate) mod testing {

    use crate::impls::dep_trackers::RecordingDepsTracker;
    use crate::impls::key::DiceKey;
    use crate::HashSet;

    pub(crate) trait RecordingDepsTrackersExt {
        fn recorded_deps(&self) -> &HashSet<DiceKey>;
    }

    impl RecordingDepsTrackersExt for RecordingDepsTracker {
        fn recorded_deps(&self) -> &HashSet<DiceKey> {
            &self.deps
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::impls::dep_trackers::RecordingDepsTracker;
    use crate::impls::key::DiceKey;
    use crate::HashSet;

    #[tokio::test]
    async fn recording_deps_tracker_tracks_deps() -> anyhow::Result<()> {
        let mut deps_tracker = RecordingDepsTracker::new();

        deps_tracker.record(DiceKey { index: 2 });
        deps_tracker.record(DiceKey { index: 3 });

        let deps = deps_tracker.collect_deps();
        let expected = HashSet::from_iter([DiceKey { index: 2 }, DiceKey { index: 3 }]);
        assert_eq!(deps, expected);

        Ok(())
    }
}
