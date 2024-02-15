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
use crate::impls::value::DiceValidity;
use crate::HashSet;

/// The 'DepsTracker' is used to record dependencies of a particular compute node by calling
/// 'record' for each dependency, and then getting a list of 'Dependency's at the end by calling
/// 'collect_deps'.
#[derive(Allocative)]
pub(crate) struct RecordingDepsTracker {
    deps: HashSet<DiceKey>,
    deps_validity: DiceValidity,
}

impl RecordingDepsTracker {
    pub(crate) fn new() -> Self {
        Self {
            deps: HashSet::default(),
            deps_validity: DiceValidity::Valid,
        }
    }

    pub(crate) fn record(&mut self, k: DiceKey, validity: DiceValidity) {
        self.deps.insert(k);
        match validity {
            DiceValidity::Transient => self.deps_validity = validity,
            _ => {}
        }
    }

    pub(crate) fn collect_deps(self) -> (HashSet<DiceKey>, DiceValidity) {
        (self.deps, self.deps_validity)
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
    use crate::impls::value::DiceValidity;
    use crate::HashSet;

    #[tokio::test]
    async fn recording_deps_tracker_tracks_deps() -> anyhow::Result<()> {
        let mut deps_tracker = RecordingDepsTracker::new();

        deps_tracker.record(DiceKey { index: 2 }, DiceValidity::Valid);
        deps_tracker.record(DiceKey { index: 3 }, DiceValidity::Valid);

        let (deps, validity) = deps_tracker.collect_deps();
        let expected = HashSet::from_iter([DiceKey { index: 2 }, DiceKey { index: 3 }]);
        assert_eq!(deps, expected);
        assert_eq!(validity, DiceValidity::Valid);

        Ok(())
    }

    #[test]
    fn recording_deps_tracker_tracks_deps_invalid() -> anyhow::Result<()> {
        let mut deps_tracker = RecordingDepsTracker::new();

        deps_tracker.record(DiceKey { index: 2 }, DiceValidity::Valid);
        deps_tracker.record(DiceKey { index: 3 }, DiceValidity::Transient);

        let (deps, validity) = deps_tracker.collect_deps();
        let expected = HashSet::from_iter([DiceKey { index: 2 }, DiceKey { index: 3 }]);
        assert_eq!(deps, expected);
        assert_eq!(validity, DiceValidity::Transient);

        Ok(())
    }
}
