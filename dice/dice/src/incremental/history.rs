/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp;
use std::cmp::min;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::Bound;
use std::fmt::Debug;

use gazebo::prelude::*;
use gazebo::variants::VariantName;

use crate::incremental::graph::ReadOnlyHistory;
use crate::incremental::versions::VersionNumber;
use crate::incremental::versions::VersionRange;
use crate::incremental::versions::VersionRanges;

/// The history of one computation unit.
/// The history is one of the `HistoryState`s.
///
/// The semantics of `CellHistory` is such that the state is Unknown for all versions until a
/// particular version `v0` is verified, upon which for all versions `v1` where `v1 > v0`, `v1 < d`
/// where `d` is the minimum dirtied version larger than `v0`, the state is Verified.
/// This is technically better represented as a single vec of history states, but we'll change the
/// actual representation later.
// TODO(bobyf): this data structure can probably be way better optimized
#[derive(Debug)]
pub(crate) struct CellHistory {
    verified: BTreeSet<VersionNumber>,
    /// versions of dirty, mapping ot whether or not it's a forced dirty (which means recompute
    /// regardless of node changed)
    dirtied: BTreeMap<VersionNumber, bool>,
}

impl CellHistory {
    pub(crate) fn verified(verified: VersionNumber) -> Self {
        Self {
            verified: btreeset![verified],
            dirtied: BTreeMap::new(),
        }
    }

    #[cfg(test)]
    pub(crate) fn dirtied(dirty: VersionNumber, force: bool) -> Self {
        Self {
            verified: BTreeSet::new(),
            dirtied: btreemap![dirty => force],
        }
    }

    pub(crate) fn empty() -> Self {
        Self {
            verified: BTreeSet::new(),
            dirtied: BTreeMap::new(),
        }
    }

    /// Makes a duplicate version of the history, and marks the given version as where a new value
    /// was verified. If applicable, another version is returned, representing the version at which
    /// the original history that is newer than the current recorded history.
    ///
    /// |earliest_valid| is the earliest possible validated version for the new history.
    /// This is usually determined by the latest verified version that is relevant to
    /// the current verified_at amongst all the dependencies.
    ///
    /// That is, given a history with state `[verified, dirty, unknown, dirty, verified, dirty]`,
    /// and `recorded_at = 2`, we would create a new history that is
    /// `[unknown, verified, verified, dirty, dirty]`. The portion of original history that's
    /// newer would be `[unknown, unknown, unknown, unknown, verified, dirty]`.
    /// Returns the version `d2`, the version that is newer if any, and the new history.
    ///
    /// This function also accounts for propagating dirtiness based on history from dependencies.
    /// See `propagate_dirty_deps`.
    pub(crate) fn make_new_verified_history(
        &self,
        verified_at: VersionNumber,
        earliest_valid: Option<VersionNumber>,
    ) -> (VersionNumber, Option<VersionNumber>, Self) {
        let mut verified = self.verified.clone();
        let mut dirtied = self.dirtied.clone();

        // If we don't have any bounds on the earliest this version can be verified,
        // we assume that it is being set at just the current version.
        let since = self.min_validatable_version(verified_at, earliest_valid);
        {
            for vt in verified
                .range((Bound::Unbounded, Bound::Excluded(since)))
                .duped()
                .collect::<Vec<_>>()
            {
                verified.remove(&vt);
            }

            for vt in dirtied
                .range((Bound::Unbounded, Bound::Excluded(since)))
                .map(|e| *e.0)
                .collect::<Vec<_>>()
            {
                dirtied.remove(&vt);
            }
        }

        let up_to = verified
            .range((Bound::Excluded(verified_at), Bound::Unbounded))
            .min()
            .copied();

        if let Some(v) = up_to.as_ref() {
            for vt in verified
                .range((Bound::Included(*v), Bound::Unbounded))
                .copied()
                .collect::<Vec<_>>()
            {
                verified.remove(&vt);
            }
        };

        verified.insert(since);
        dirtied.remove(&since);

        let new = CellHistory { verified, dirtied };

        (since, up_to, new)
    }

    /// Marks the given version as verified on the history, returning the oldest version that
    /// became verified due to marking this node as verified.
    /// For example, assuming no deps, if dirtied at v2, and marking v4, the oldest version that
    /// became verified would be v2, since marking v4 with no changes from v2 to v4 implies that
    /// all of v2, v3, v4 are verified.
    /// But if instead there was a dep v3, v2 could not be marked verified, but v3 & v4 would be.
    ///
    /// Dependencies history are accounted for by propagating their dirtied versions to the
    /// verified history through 'propagate_dirty_from_deps'
    pub(crate) fn mark_verified<'a, I>(&mut self, v: VersionNumber, deps: I) -> VersionNumber
    where
        I: IntoIterator<Item = ReadOnlyHistory<'a>>,
        I::IntoIter: Clone,
    {
        let deps_iter = deps.into_iter();
        // We can't be verified before any of our deps were most-recently verified.
        let all_deps_unchanged_since = deps_iter
            .clone()
            .filter_map(|dep| dep.latest_verified_before(v))
            .max();
        let min_validated = self.min_validatable_version(v, all_deps_unchanged_since);
        let changed_since = if let Some(prev_verified) = self
            .verified
            .range((Bound::Excluded(min_validated), Bound::Included(v)))
            .next()
        {
            *prev_verified
        } else {
            self.dirtied.remove(&min_validated);
            self.verified.insert(min_validated);
            min_validated
        };

        self.propagate_from_deps(changed_since, deps_iter);

        changed_since
    }

    /// return true if the history was changed, else false.
    pub(crate) fn mark_invalidated(&mut self, v: VersionNumber) -> bool {
        if self.dirtied.contains_key(&v) {
            return false;
        }

        self.dirty(v, false);
        true
    }

    /// Return true if the history was changed, else false.
    /// This forces this node to be invalidated and history to be "dirty" so that it's recomputed
    /// regardless if dependencies changed
    pub(crate) fn force_dirty(&mut self, v: VersionNumber) -> bool {
        if self.dirtied.get(&v).map_or(false, |d| *d) {
            // only noop if this was already force dirtied, otherwise, we override the dirty with
            // force dirty
            false
        } else {
            self.dirty(v, true);
            true
        }
    }

    /// returns a vec of ranges of verified versions. This is returned as a 'VersionRange'
    pub(crate) fn get_verified_ranges(&self) -> VersionRanges {
        let mut verified = self.verified.iter().peekable();
        let mut dirtied = self.dirtied.iter().peekable();

        let mut out = VersionRanges::new();
        let mut last_verified = None;
        loop {
            match (verified.peek(), dirtied.peek()) {
                (Some(v), Some(d)) => {
                    if v < &d.0 {
                        last_verified.get_or_insert(*verified.next().expect("we just peeked it"));
                    } else if let Some(begin) = last_verified.take() {
                        out.insert(VersionRange::bounded(
                            begin,
                            *dirtied.next().expect("we just peeked it").0,
                        ));
                    } else {
                        dirtied.next();
                    }
                }
                (Some(_), None) => {
                    out.insert(VersionRange::begins_with(
                        last_verified.unwrap_or(*verified.next().expect("we just peeked it")),
                    ));
                    return out;
                }
                (None, Some(_)) => {
                    if let Some(begin) = last_verified.take() {
                        out.insert(VersionRange::bounded(
                            begin,
                            *dirtied.next().expect("we just peeked it").0,
                        ));
                    } else {
                        dirtied.next();
                    }
                    return out;
                }
                (None, None) => {
                    if let Some(begin) = last_verified {
                        out.insert(VersionRange::begins_with(begin))
                    }
                    return out;
                }
            }
        }
    }

    pub(crate) fn get_history(&self, v: &VersionNumber) -> HistoryState {
        if let Some(last_verified) = self
            .verified
            .range((Bound::Unbounded, Bound::Included(*v)))
            .max()
            .copied()
        {
            if let Some((_, is_force)) = self
                .dirtied
                .range((Bound::Included(last_verified), Bound::Included(*v)))
                .next()
            {
                if *is_force {
                    HistoryState::Dirty
                } else {
                    HistoryState::Unknown(self.get_verified_ranges())
                }
            } else {
                HistoryState::Verified
            }
        } else {
            HistoryState::Unknown(self.get_verified_ranges())
        }
    }

    pub(crate) fn latest_dirtied(&self) -> Option<VersionNumber> {
        self.dirtied.iter().max().map(|d| *d.0)
    }

    pub(crate) fn latest_verified_before(&self, v: VersionNumber) -> Option<VersionNumber> {
        self.verified
            .range((Bound::Unbounded, Bound::Included(v)))
            .next_back()
            .copied()
    }

    /// When a node is recomputed to the same value as its existing history, but with a new set of
    /// dependencies, that node needs to know when itself will next be dirtied due to changes in its
    /// new dependencies.
    /// This will make the current history propagate any dirty versions necessary from the given
    /// set of dependencies at a version 'v'.
    pub(crate) fn propagate_from_deps<'a>(
        &mut self,
        v: VersionNumber,
        deps: impl IntoIterator<Item = ReadOnlyHistory<'a>>,
    ) {
        // By verifying the given version, we only need to fill in the history up to the next
        // smallest verified and dirtied version. Any dirties beyond that would be irrelevant
        // as either we would already be dirtied, or some newer version have already verified us
        // and these propagated deps are irrelevant.
        let relevant_hist_up_to = {
            let nearest_verified = self
                .verified
                .range((Bound::Excluded(v), Bound::Unbounded))
                .next();
            let nearest_dirted = self
                .dirtied
                .range((Bound::Excluded(v), Bound::Unbounded))
                .next()
                .map(|e| e.0);

            match (nearest_verified, nearest_dirted) {
                (Some(v), Some(d)) => Some(*min(v, d)),
                (Some(v), None) => Some(*v),
                (None, Some(d)) => Some(*d),
                (None, None) => None,
            }
        };

        // We only need to propagate the earliest “dirty” from any of its deps, as we can rely on
        // the recomputation from that dirty version to re-propagate any newer “dirty” as needed.
        let mut min_dirty = None;
        for dep in deps {
            if let Some(dirty) = dep
                .dirtied
                .range((
                    Bound::Excluded(v),
                    relevant_hist_up_to.map_or(Bound::Unbounded, Bound::Excluded),
                ))
                .next()
                .map(|e| *e.0)
            {
                min_dirty = min_dirty.map_or(Some(dirty), |old| Some(cmp::min(old, dirty)))
            }
        }

        if let Some(min_dirty) = min_dirty {
            self.dirtied.insert(min_dirty, false);
        }
    }

    fn min_validatable_version(
        &self,
        verified_at: VersionNumber,
        earliest_valid: Option<VersionNumber>,
    ) -> VersionNumber {
        let last_dirtied = self
            .dirtied
            .range((Bound::Unbounded, Bound::Included(verified_at)))
            .next_back()
            .map(|r| *r.0);
        // If we don't have any bounds on the earliest this version can be verified,
        // we assume that it is being set at just the current version.
        [last_dirtied, earliest_valid]
            .into_iter()
            .flatten()
            .max()
            .unwrap_or(verified_at)
    }
}

impl CellHistory {
    fn dirty(&mut self, v: VersionNumber, force: bool) {
        assert!(
            !self
                .verified
                .range((Bound::Included(v), Bound::Unbounded))
                .any(|_| true),
            "should never get into state where we are dirtying a version `{:?}` that was explicitly marked as verified. Verified versions `{:?}`",
            v,
            self.verified
        );

        self.dirtied.insert(v, force);
    }
}

/// The various different states that a particular 'VersionNumber' can be in with respect to a
/// 'CellHistory'
#[derive(VariantName)]
pub(crate) enum HistoryState {
    /// known to be verified
    Verified,
    /// version is in unknown state, where the last known verified version is returned
    Unknown(VersionRanges),
    /// version is known to be dirty
    Dirty,
}

#[cfg(test)]
pub(crate) mod testing {
    use gazebo::variants::VariantName;

    use crate::incremental::history::CellHistory;
    use crate::incremental::history::HistoryState;
    use crate::incremental::VersionNumber;
    use crate::incremental::VersionRanges;

    pub(crate) trait CellHistoryExt {
        fn testing_new(verified: &[VersionNumber], dirtied: &[VersionNumber]) -> Self;

        fn get_verified(&self) -> Vec<VersionNumber>;
    }

    impl CellHistoryExt for CellHistory {
        fn testing_new(verified: &[VersionNumber], dirtied: &[VersionNumber]) -> Self {
            CellHistory {
                verified: verified.iter().copied().collect(),
                dirtied: dirtied.iter().map(|v| (*v, false)).collect(),
            }
        }

        fn get_verified(&self) -> Vec<VersionNumber> {
            self.verified.iter().copied().collect()
        }
    }

    pub(crate) trait HistoryExt {
        fn assert_verified(&self);

        fn assert_unknown(&self) -> &VersionRanges;

        fn assert_dirty(&self);
    }

    impl HistoryExt for HistoryState {
        fn assert_verified(&self) {
            match self {
                HistoryState::Verified => {}
                x => panic!("expected Verified but was {}", x.variant_name()),
            }
        }

        fn assert_unknown(&self) -> &VersionRanges {
            match self {
                HistoryState::Unknown(v) => v,
                x => panic!("expected Unknown but was {}", x.variant_name()),
            }
        }

        fn assert_dirty(&self) {
            match self {
                HistoryState::Dirty => {}
                x => panic!("expected Dirty but was {}", x.variant_name()),
            }
        }
    }
}

mod introspection {
    use crate::incremental::history::CellHistory;
    use crate::incremental::versions::VersionNumber;

    impl CellHistory {
        pub fn to_introspectable(&self) -> crate::introspection::graph::CellHistory {
            crate::introspection::graph::CellHistory::new(
                self.verified
                    .iter()
                    .map(VersionNumber::to_introspectable)
                    .collect(),
                self.dirtied
                    .iter()
                    .map(|(k, v)| (k.to_introspectable(), *v))
                    .collect(),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use crate::incremental::graph::ReadOnlyHistory;
    use crate::incremental::history::testing::CellHistoryExt;
    use crate::incremental::history::testing::HistoryExt;
    use crate::incremental::history::CellHistory;
    use crate::incremental::versions::testing::VersionRangesExt;
    use crate::incremental::versions::VersionRange;
    use crate::incremental::versions::VersionRanges;
    use crate::incremental::VersionNumber;

    #[test]
    fn cell_history_propagates() {
        let mut hist = CellHistory::verified(VersionNumber::new(0));
        hist.propagate_from_deps(
            VersionNumber::new(0),
            vec![
                ReadOnlyHistory::TestingValue(CellHistory::dirtied(VersionNumber::new(1), false)),
                ReadOnlyHistory::TestingValue(CellHistory::dirtied(VersionNumber::new(4), true)),
            ],
        );
        assert_eq!(
            hist.get_history(&VersionNumber::new(1)).assert_unknown(),
            &VersionRanges::testing_new(btreeset![VersionRange::bounded(
                VersionNumber::new(0),
                VersionNumber::new(1)
            )])
        );

        // now verify it if the history was already verified at some future versions.
        let mut hist = CellHistory {
            verified: btreeset![VersionNumber::new(0), VersionNumber::new(2)],
            dirtied: Default::default(),
        };
        // we should ignore dirties that occur after the known version
        hist.propagate_from_deps(
            VersionNumber::new(0),
            vec![ReadOnlyHistory::TestingValue(CellHistory::dirtied(
                VersionNumber::new(4),
                false,
            ))],
        );
        hist.get_history(&VersionNumber::new(2)).assert_verified();
        hist.get_history(&VersionNumber::new(4)).assert_verified();

        // we should propagate dirties that occur before the known version
        hist.propagate_from_deps(
            VersionNumber::new(0),
            vec![ReadOnlyHistory::TestingValue(CellHistory::dirtied(
                VersionNumber::new(1),
                false,
            ))],
        );
        assert_eq!(
            hist.get_history(&VersionNumber::new(1)).assert_unknown(),
            &VersionRanges::testing_new(btreeset![
                VersionRange::bounded(VersionNumber::new(0), VersionNumber::new(1)),
                VersionRange::begins_with(VersionNumber::new(2))
            ])
        );

        // now verify it if the history was already dirted at some future versions.
        let mut hist = CellHistory {
            verified: btreeset![VersionNumber::new(0), VersionNumber::new(2)],
            dirtied: Default::default(),
        };
        // we should ignore dirties that occur after the known version
        hist.propagate_from_deps(
            VersionNumber::new(0),
            vec![ReadOnlyHistory::TestingValue(CellHistory::dirtied(
                VersionNumber::new(4),
                false,
            ))],
        );
        hist.get_history(&VersionNumber::new(2)).assert_verified();
        hist.get_history(&VersionNumber::new(4)).assert_verified();

        // we should propagate dirties that occur before the known version
        hist.propagate_from_deps(
            VersionNumber::new(0),
            vec![ReadOnlyHistory::TestingValue(CellHistory::dirtied(
                VersionNumber::new(1),
                false,
            ))],
        );
        assert_eq!(
            hist.get_history(&VersionNumber::new(1)).assert_unknown(),
            &VersionRanges::testing_new(btreeset![
                VersionRange::bounded(VersionNumber::new(0), VersionNumber::new(1)),
                VersionRange::begins_with(VersionNumber::new(2))
            ])
        );
    }

    #[test]
    fn cell_history_tracks_correctly() {
        let mut history = CellHistory::verified(VersionNumber::new(0));
        history
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(1))
            .assert_verified();

        assert_eq!(history.mark_invalidated(VersionNumber::new(2)), true);
        assert_eq!(history.mark_invalidated(VersionNumber::new(2)), false);
        assert_eq!(
            history.get_history(&VersionNumber::new(2)).assert_unknown(),
            &VersionRanges::testing_new(btreeset![VersionRange::bounded(
                VersionNumber::new(0),
                VersionNumber::new(2)
            )])
        );

        assert_eq!(
            history.mark_verified(VersionNumber::new(0), std::iter::empty()),
            VersionNumber::new(0)
        );
        history
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(1))
            .assert_verified();
        assert_eq!(
            history.get_history(&VersionNumber::new(2)).assert_unknown(),
            &VersionRanges::testing_new(btreeset![VersionRange::bounded(
                VersionNumber::new(0),
                VersionNumber::new(2),
            )])
        );

        assert_eq!(
            history.mark_verified(VersionNumber::new(2), std::iter::empty()),
            VersionNumber::new(2)
        );
        history
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(2))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(1))
            .assert_verified();

        assert_eq!(history.mark_invalidated(VersionNumber::new(3)), true);
        history
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(2))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(1))
            .assert_verified();
        assert_eq!(
            history.get_history(&VersionNumber::new(3)).assert_unknown(),
            &VersionRanges::testing_new(btreeset![VersionRange::bounded(
                VersionNumber::new(0),
                VersionNumber::new(3)
            )])
        );
        assert_eq!(
            history.get_history(&VersionNumber::new(5)).assert_unknown(),
            &VersionRanges::testing_new(btreeset![VersionRange::bounded(
                VersionNumber::new(0),
                VersionNumber::new(3)
            )])
        );

        let up_to = history.mark_verified(
            VersionNumber::new(4),
            [VersionNumber::new(5), VersionNumber::new(8)]
                .into_iter()
                .map(|v| ReadOnlyHistory::TestingValue(CellHistory::dirtied(v, false))),
        );
        assert_eq!(up_to, VersionNumber::new(3));
        history
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(2))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(3))
            .assert_verified();
        assert_eq!(
            history.get_history(&VersionNumber::new(5)).assert_unknown(),
            &VersionRanges::testing_new(btreeset![VersionRange::bounded(
                VersionNumber::new(0),
                VersionNumber::new(5)
            )])
        );

        assert_eq!(history.mark_invalidated(VersionNumber::new(6)), true);
        let up_to = history.mark_verified(VersionNumber::new(7), std::iter::empty());
        assert_eq!(up_to, VersionNumber::new(6));
        history
            .get_history(&VersionNumber::new(0))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(2))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(3))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(6))
            .assert_verified();
        history
            .get_history(&VersionNumber::new(7))
            .assert_verified();
        assert_eq!(
            history.get_history(&VersionNumber::new(5)).assert_unknown(),
            &VersionRanges::testing_new(btreeset![
                VersionRange::bounded(VersionNumber::new(0), VersionNumber::new(5)),
                VersionRange::begins_with(VersionNumber::new(6))
            ])
        );

        assert!(history.mark_invalidated(VersionNumber::new(9)));
        assert!(!history.dirtied.get(&VersionNumber::new(9)).unwrap());
        assert!(history.force_dirty(VersionNumber::new(9)));
        assert!(history.dirtied.get(&VersionNumber::new(9)).unwrap());
        assert!(!history.force_dirty(VersionNumber::new(9)));
        history
            .get_history(&VersionNumber::new(7))
            .assert_verified();
        history.get_history(&VersionNumber::new(9)).assert_dirty();
        history.get_history(&VersionNumber::new(10)).assert_dirty();

        // Here, since one dep is only verified at v11, we can't mark v10 as verified.
        let up_to = history.mark_verified(
            VersionNumber::new(11),
            [VersionNumber::new(10), VersionNumber::new(11)]
                .into_iter()
                .map(|v| ReadOnlyHistory::TestingValue(CellHistory::verified(v))),
        );
        assert_eq!(up_to, VersionNumber::new(11));

        history.get_history(&VersionNumber::new(9)).assert_dirty();
        history.get_history(&VersionNumber::new(10)).assert_dirty();
        history
            .get_history(&VersionNumber::new(11))
            .assert_verified();
    }

    #[test]
    fn cell_history_makes_new_history_correctly() {
        let hist = CellHistory::testing_new(
            &[VersionNumber::new(0), VersionNumber::new(3)],
            &[VersionNumber::new(1), VersionNumber::new(4)],
        );
        let (v, end, hist) = hist.make_new_verified_history(VersionNumber::new(2), None);

        assert_eq!(end, Some(VersionNumber::new(3)));
        assert_eq!(v, VersionNumber::new(1));
        assert_eq!(hist.dirtied, btreemap![VersionNumber::new(4) => false]);
        assert_eq!(hist.verified, btreeset![VersionNumber::new(1)],);
    }

    #[test]
    fn cell_history_verified_ranges() {
        let hist = CellHistory::testing_new(&[], &[]);
        assert_eq!(hist.get_verified_ranges().ranges(), &BTreeSet::new());

        let hist = CellHistory::testing_new(&[VersionNumber::new(1)], &[]);
        assert_eq!(
            hist.get_verified_ranges().ranges(),
            &btreeset![VersionRange::begins_with(VersionNumber::new(1))]
        );

        let hist = CellHistory::testing_new(&[VersionNumber::new(1), VersionNumber::new(3)], &[]);
        assert_eq!(
            hist.get_verified_ranges().ranges(),
            &btreeset![VersionRange::begins_with(VersionNumber::new(1))]
        );

        let hist = CellHistory::testing_new(
            &[VersionNumber::new(1), VersionNumber::new(3)],
            &[VersionNumber::new(2)],
        );
        assert_eq!(
            hist.get_verified_ranges().ranges(),
            &btreeset![
                VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(2)),
                VersionRange::begins_with(VersionNumber::new(3))
            ]
        );

        let hist = CellHistory::testing_new(&[VersionNumber::new(1)], &[VersionNumber::new(3)]);
        assert_eq!(
            hist.get_verified_ranges().ranges(),
            &btreeset![VersionRange::bounded(
                VersionNumber::new(1),
                VersionNumber::new(3)
            ),]
        );

        let hist = CellHistory::testing_new(
            &[
                VersionNumber::new(1),
                VersionNumber::new(2),
                VersionNumber::new(4),
                VersionNumber::new(7),
                VersionNumber::new(9),
            ],
            &[
                VersionNumber::new(3),
                VersionNumber::new(5),
                VersionNumber::new(6),
            ],
        );
        assert_eq!(
            hist.get_verified_ranges().ranges(),
            &btreeset![
                VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(3)),
                VersionRange::bounded(VersionNumber::new(4), VersionNumber::new(5)),
                VersionRange::begins_with(VersionNumber::new(7))
            ]
        );
    }
}
