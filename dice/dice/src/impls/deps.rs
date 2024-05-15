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
use itertools::Either;
use static_assertions::assert_eq_size;
use typed_arena::Arena;

use crate::impls::key::DiceKey;
use crate::impls::value::DiceValidity;
use crate::HashSet;

/// The 'DepsTracker' is used to record dependencies of a particular compute node by calling
/// 'record' for each dependency, and then getting a list of 'Dependency's at the end by calling
/// 'collect_deps'.
#[derive(Allocative)]
pub(crate) struct RecordingDepsTracker {
    deps: RecordedDeps,

    /// While a parallel computation is happening (from ctx.compute_many()/etc), we'll have an Arena here
    /// where each parallel ctx gets a slot for its deps. After the parallel computation is finished, we'll
    /// then record this into deps above.
    #[allocative(skip)] // TODO(cjhopman): Fix this.
    curr_parallel: Option<Box<SyncArena<RecordedDeps>>>,
}

#[derive(Allocative)]
pub(crate) struct RecordedDeps {
    pub(crate) deps: SmallDepsList,
    pub(crate) deps_validity: DiceValidity,
}

/// It's very common for a parallel compute to record only a single dep and so we optimize
/// for that with this simple list.
#[derive(Allocative)]
pub(crate) enum SmallDepsList {
    None,
    One(DiceKey),
    #[allow(clippy::box_collection)]
    Many(Box<Vec<DiceKey>>),
}

impl SmallDepsList {
    fn new() -> SmallDepsList {
        SmallDepsList::None
    }

    fn insert(&mut self, k: DiceKey) {
        match self {
            SmallDepsList::None => *self = SmallDepsList::One(k),
            SmallDepsList::One(_) => {
                let v = std::mem::replace(self, SmallDepsList::None);
                let v = match v {
                    SmallDepsList::One(v) => v,
                    _ => unreachable!(),
                };
                *self = SmallDepsList::Many(Box::new(vec![v, k]));
            }
            SmallDepsList::Many(v) => v.push(k),
        }
    }

    fn reserve(&mut self, to_reserve: usize) -> &mut Vec<DiceKey> {
        match self {
            SmallDepsList::None => {
                *self = SmallDepsList::Many(Box::new(Vec::with_capacity(to_reserve)));
            }
            SmallDepsList::One(_) => {
                let v = std::mem::replace(self, SmallDepsList::None);
                let v = match v {
                    SmallDepsList::One(v) => v,
                    _ => unreachable!(),
                };
                let mut new = Vec::with_capacity(1 + to_reserve);
                new.push(v);
                *self = SmallDepsList::Many(Box::new(new));
            }
            SmallDepsList::Many(k) => k.reserve(to_reserve),
        }

        match self {
            SmallDepsList::Many(v) => &mut *v,
            _ => unreachable!(),
        }
    }

    fn len(&self) -> usize {
        match self {
            SmallDepsList::None => 0,
            SmallDepsList::One(_) => 1,
            SmallDepsList::Many(v) => v.len(),
        }
    }

    pub(crate) fn into_set(self) -> HashSet<DiceKey> {
        match self {
            SmallDepsList::None => HashSet::default(),
            SmallDepsList::One(v) => {
                let mut s = HashSet::default();
                s.insert(v);
                s
            }
            SmallDepsList::Many(v) => v.into_iter().collect(),
        }
    }

    pub(crate) fn iter_keys(&self) -> impl Iterator<Item = DiceKey> + '_ {
        match self {
            SmallDepsList::None => Either::Left(Option::<DiceKey>::None.into_iter()),
            SmallDepsList::One(v) => Either::Left(Option::<DiceKey>::Some(*v).into_iter()),
            SmallDepsList::Many(m) => Either::Right(m.iter().copied()),
        }
    }
}

impl RecordedDeps {
    fn record(&mut self, k: DiceKey, validity: DiceValidity) {
        self.deps.insert(k);
        self.deps_validity.and(validity);
    }

    pub(crate) fn new() -> Self {
        RecordedDeps {
            deps: SmallDepsList::new(),
            deps_validity: DiceValidity::Valid,
        }
    }

    #[allow(unused)] // TODO(cjhopman): remove this
    pub(crate) fn iter_keys(&self) -> impl Iterator<Item = DiceKey> + '_ {
        self.deps.iter_keys()
    }
}

assert_eq_size!(RecordingDepsTracker, [usize; 4]);

fn _check_deps_trackers_send_and_sync() {
    fn _assert_send_sync<T: Send + Sync>() {}
    _assert_send_sync::<RecordingDepsTracker>();
}

impl RecordingDepsTracker {
    pub(crate) fn new() -> Self {
        Self {
            deps: RecordedDeps::new(),
            curr_parallel: None,
        }
    }

    pub(crate) fn record(&mut self, k: DiceKey, validity: DiceValidity) {
        self.flatten_parallel();
        self.deps.record(k, validity)
    }

    /// Used to start a new parallel computation. Returns the Arena that each parallel ctx should record its deps to.
    pub(crate) fn push_parallel(&mut self, size: usize) -> &Arena<RecordedDeps> {
        self.flatten_parallel();
        assert!(self.curr_parallel.is_none());
        self.curr_parallel
            .insert(Box::new(SyncArena::with_capacity(size)))
            .inner()
    }

    pub(crate) fn collect_deps(mut self) -> RecordedDeps {
        self.flatten_parallel();
        self.deps
    }

    /// "Flattens" the previous parallel computation into deps if there is one. This should be called on any function
    /// that accesses/writes to deps.
    fn flatten_parallel(&mut self) {
        if let Some(mut parallel) = self.curr_parallel.take() {
            let mut to_reserve = 0;
            for d in parallel.iter_mut() {
                to_reserve += d.deps.len();
                self.deps.deps_validity.and(d.deps_validity);
            }
            let reserved = self.deps.deps.reserve(to_reserve);
            for d in parallel.iter_mut() {
                let d = std::mem::replace(&mut d.deps, SmallDepsList::None);
                match d {
                    SmallDepsList::None => {}
                    SmallDepsList::One(one) => reserved.push(one),
                    SmallDepsList::Many(many) => reserved.extend(many.into_iter()),
                }
            }
        }
    }
}

mod sync_arena {
    // We put SyncArena in its own mod to make the inner Arena truly private.
    use typed_arena::Arena;

    pub(super) struct SyncArena<T>(Arena<T>);

    /// Safety: SyncArena only exposes apis taking `&mut self` and `self`.
    unsafe impl<T: Sync> Sync for SyncArena<T> {}

    impl<T> SyncArena<T> {
        pub(super) fn with_capacity(s: usize) -> Self {
            Self(Arena::with_capacity(s))
        }

        pub(super) fn inner(&mut self) -> &Arena<T> {
            &self.0
        }

        pub(super) fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
            self.0.iter_mut()
        }
    }
}
use sync_arena::SyncArena;

#[cfg(test)]
pub(crate) mod testing {

    use crate::impls::deps::RecordingDepsTracker;
    use crate::impls::key::DiceKey;
    use crate::HashSet;

    pub(crate) trait RecordingDepsTrackersExt {
        fn recorded_deps(&self) -> HashSet<DiceKey>;
    }

    impl RecordingDepsTrackersExt for RecordingDepsTracker {
        fn recorded_deps(&self) -> HashSet<DiceKey> {
            self.deps.iter_keys().collect()
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::impls::deps::RecordingDepsTracker;
    use crate::impls::key::DiceKey;
    use crate::impls::value::DiceValidity;
    use crate::HashSet;

    #[tokio::test]
    async fn recording_deps_tracker_tracks_deps() -> anyhow::Result<()> {
        let mut deps_tracker = RecordingDepsTracker::new();

        deps_tracker.record(DiceKey { index: 2 }, DiceValidity::Valid);
        deps_tracker.record(DiceKey { index: 3 }, DiceValidity::Valid);

        let recorded_deps = deps_tracker.collect_deps();
        let expected = HashSet::from_iter([DiceKey { index: 2 }, DiceKey { index: 3 }]);
        assert_eq!(recorded_deps.deps.into_set(), expected);
        assert_eq!(recorded_deps.deps_validity, DiceValidity::Valid);

        Ok(())
    }

    #[test]
    fn recording_deps_tracker_tracks_deps_invalid() -> anyhow::Result<()> {
        let mut deps_tracker = RecordingDepsTracker::new();

        deps_tracker.record(DiceKey { index: 2 }, DiceValidity::Valid);
        deps_tracker.record(DiceKey { index: 3 }, DiceValidity::Transient);

        let recorded_deps = deps_tracker.collect_deps();
        let expected = HashSet::from_iter([DiceKey { index: 2 }, DiceKey { index: 3 }]);
        assert_eq!(recorded_deps.deps.into_set(), expected);
        assert_eq!(recorded_deps.deps_validity, DiceValidity::Transient);

        Ok(())
    }
}
