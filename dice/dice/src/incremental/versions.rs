/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! This library contains utilities for tracking a global version number. The
//! global version number is used for tagging computed values so that we can
//! track when a value needs to be updated because its version number is out of
//! date.

use std::cell::UnsafeCell;
use std::cmp;
use std::collections::BTreeSet;
use std::collections::Bound;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::Deref;
use std::ops::RangeBounds;
use std::ops::Sub;
use std::sync::Arc;
use std::sync::RwLock;
use std::sync::Weak;

use derive_more::Display;
use gazebo::prelude::*;
use once_cell::sync::OnceCell;
use parking_lot::lock_api::RawMutex as RawMutexApi;
use parking_lot::RawMutex;
use serde::Deserialize;
use serde::Serialize;

/// The incrementing Version number associated with all the cache entries
#[derive(Copy, Eq, Debug, Display, Dupe)]
// split this due to formatters not agreeing
#[derive(PartialEq, Hash, Clone, Ord, PartialOrd, Serialize, Deserialize)]
#[display(fmt = "v{}", "_0")]
#[serde(transparent)]
pub(crate) struct VersionNumber(usize);

impl VersionNumber {
    /// First transaction has version number zero.
    const ZERO: VersionNumber = VersionNumber(0);

    pub(crate) fn new(num: usize) -> Self {
        VersionNumber(num)
    }

    fn inc(&mut self) {
        self.0 += 1;
    }
}

impl Sub for VersionNumber {
    type Output = isize;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0 as isize - rhs.0 as isize
    }
}

/// The minor version associated with the major `VersionNumber`. The minor version an identifier to
/// all currently active computations of a particular `VersionNumber`.
/// So, for all computations currently active at a particular `VersionNumber`, they all share the
/// same `MinorVersion`. Furthermore, each time a computation occurs at a `VersionNumber`, if there
/// are no currently active computations at that `VersionNumber`, the `MinorVersion` is increased.
#[derive(Copy, Eq, Debug, Display, Dupe)]
// split this due to formatters not agreeing
#[derive(PartialEq, Hash, Clone, Ord, PartialOrd)]
#[display(fmt = "m{}", "_0")]
pub(crate) struct MinorVersion(usize);

impl MinorVersion {
    const ZERO: MinorVersion = MinorVersion(0);

    pub(crate) fn next(&self) -> MinorVersion {
        MinorVersion(self.0 + 1)
    }
}

#[cfg(test)]
impl MinorVersion {
    pub(crate) fn testing_new(num: usize) -> Self {
        MinorVersion(num)
    }
}

/// Represents a range of versions. This range must have a start that is inclusive, but may be
/// unbounded towards the end. The end, if present, is exclusive.
#[derive(Eq, Debug, Dupe, PartialEq, Hash, Clone)]
pub(crate) struct VersionRange {
    begin: VersionNumber,
    end: Option<VersionNumber>,
}

impl Display for VersionRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[{}, ", self.begin)?;
        match self.end {
            None => {
                write!(f, "Unbounded")?;
            }
            Some(end) => {
                write!(f, "{}", end)?;
            }
        }
        write!(f, ")")
    }
}

impl VersionRange {
    fn new(begin: VersionNumber, end: Option<VersionNumber>) -> Self {
        if let Some(end) = end {
            assert!(begin < end);
        }
        VersionRange { begin, end }
    }

    pub(crate) fn bounded(begin: VersionNumber, end: VersionNumber) -> Self {
        VersionRange::new(begin, Some(end))
    }

    pub(crate) fn begins_with(begin: VersionNumber) -> Self {
        VersionRange::new(begin, None)
    }

    pub(crate) fn intersect(&self, other: &VersionRange) -> Option<Self> {
        // we exclude the end bound, because intervals [1,2) and [2,3) do not intersect
        fn contains_end_exclusive(
            v: VersionNumber,
            begin: VersionNumber,
            end: Option<VersionNumber>,
        ) -> bool {
            v >= begin && end.map_or(true, |end| v < end)
        }

        if contains_end_exclusive(self.begin, other.begin, other.end)
            || contains_end_exclusive(other.begin, self.begin, self.end)
        {
            let begin = cmp::max(self.begin, other.begin);
            let end = match (self.end, other.end) {
                (Some(x), Some(y)) => Some(cmp::min(x, y)),
                (None, x) => x,
                (y, None) => y,
            };

            Some(VersionRange::new(begin, end))
        } else {
            None
        }
    }

    /// splits the current range into two ranges at the given version, updating the current range
    /// and returning the new range that occurs after this range. If the given version is
    /// outside the range, then this does nothing and returns None.
    #[allow(unused)] // useful function. Probably will be needed at some point in the future
    pub(crate) fn split(&mut self, at: VersionNumber) -> Option<Self> {
        if self.begin < at {
            match self.end {
                Some(end) => {
                    if at < end {
                        self.end = Some(at);
                        Some(Self::bounded(at, end))
                    } else {
                        None
                    }
                }
                None => {
                    self.end = Some(at);
                    Some(Self::begins_with(at))
                }
            }
        } else {
            None
        }
    }

    #[allow(unused)] // useful function
    pub(crate) fn begin(&self) -> &VersionNumber {
        &self.begin
    }

    /// Merges this range with the given range if they overlap, otherwise return `None`
    pub(crate) fn merge(&self, other: &VersionRange) -> Option<Self> {
        // when merging, we include the end point, because intervals of the form [1,2) and [2,3)
        // should be merged into [1,3)
        fn is_between_end_inclusive(
            v: VersionNumber,
            begin: VersionNumber,
            end: Option<VersionNumber>,
        ) -> bool {
            v >= begin && end.map_or(true, |end| v <= end)
        }

        if is_between_end_inclusive(self.begin, other.begin, other.end)
            || is_between_end_inclusive(other.begin, self.begin, self.end)
        {
            Some(VersionRange::new(
                cmp::min(self.begin, other.begin),
                match (self.end, other.end) {
                    (None, _) => None,
                    (_, None) => None,
                    (Some(e1), Some(e2)) => Some(cmp::max(e1, e2)),
                },
            ))
        } else {
            None
        }
    }
}

impl RangeBounds<VersionNumber> for VersionRange {
    fn start_bound(&self) -> Bound<&VersionNumber> {
        Bound::Included(&self.begin)
    }

    fn end_bound(&self) -> Bound<&VersionNumber> {
        self.end.as_ref().map_or(Bound::Unbounded, Bound::Included)
    }
}

impl PartialOrd for VersionRange {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// We form a total ordering for the range by first comparing the beginning version, followed by
/// the end version if the beginning is tied. Since `None` indicates unbounded, that would be
/// larger than any version.
impl Ord for VersionRange {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        cmp_chain! {
            self.begin.cmp(&other.begin),
            match (self.end, other.end) {
                (None, None) => cmp::Ordering::Equal,
                (None, Some(_)) => cmp::Ordering::Greater,
                (Some(_), None) => cmp::Ordering::Less,
                (Some(end), Some(other_end)) => end.cmp(&other_end)
            }
        }
    }
}

/// Represents a sequence of `VersionRange`s. Each range contained in this sorted set is disjoint.
/// Any operations that causes ranges to overlap will result in the ranges being merged.
/// A version is contained in the `VersionRanges` iff there exists a `VersionRange` in this
/// sequence of ranges that contains the version. That is, consider the sequence `[[1,3), [5,6)]`,
/// 3, and 4 would not be in the sequence of ranges, but 1, 2, 5, would be. This is essentially
/// a list of numerical end-exclusive intervals.
#[derive(Eq, Debug, PartialEq, Hash, Clone, Ord, PartialOrd)]
pub(crate) struct VersionRanges(BTreeSet<VersionRange>);

impl Display for VersionRanges {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (i, range) in self.0.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", range)?;
        }
        write!(f, "}}")
    }
}

impl VersionRanges {
    pub(crate) fn new() -> Self {
        Self(Default::default())
    }

    /// inserts a single range, merging different ranges if necessary
    pub(crate) fn insert(&mut self, mut range: VersionRange) {
        if let Some(smaller) = self
            .0
            .range((Bound::Unbounded, Bound::Included(range.dupe())))
            .max()
        {
            if let Some(merged) = smaller.merge(&range) {
                let to_remove = smaller.dupe();
                let removed = self.0.remove(&to_remove);
                assert!(removed);
                range = merged;
            }
        }

        if let Some(larger) = self
            .0
            .range((Bound::Included(range.dupe()), Bound::Unbounded))
            .min()
        {
            if let Some(merged) = larger.merge(&range) {
                let to_remove = larger.dupe();
                let removed = self.0.remove(&to_remove);
                assert!(removed);
                range = merged;
            }
        }

        let inserted = self.0.insert(range);
        assert!(inserted);
    }

    /// Computes the union of this set of ranges and another
    pub(crate) fn union(&self, other: &VersionRanges) -> VersionRanges {
        let mut this = self.0.iter().peekable();
        let mut other = other.0.iter().peekable();

        let mut out = BTreeSet::new();
        let mut pending: Option<VersionRange> = None;
        loop {
            let smaller = match (this.peek(), other.peek()) {
                (Some(this_range), Some(other_range)) => {
                    if this_range < other_range {
                        this.next().expect("just peeked")
                    } else {
                        other.next().expect("just peeked")
                    }
                }
                (Some(_), None) => this.next().expect("just peeked"),
                (None, Some(_)) => other.next().expect("just peeked"),
                (None, None) => break,
            };

            pending = Some(pending.map_or_else(
                || smaller.dupe(),
                |last| {
                    if let Some(merged) = last.merge(smaller) {
                        merged
                    } else {
                        out.insert(last);
                        smaller.dupe()
                    }
                },
            ));
        }

        if let Some(last) = pending {
            out.insert(last);
        }

        VersionRanges(out)
    }

    /// Computes the intersection of this set of ranges and another
    pub(crate) fn intersect(&self, other: &VersionRanges) -> VersionRanges {
        let mut this = self.0.iter().peekable();
        let mut other = other.0.iter().peekable();

        let mut out = BTreeSet::new();
        // Pending is the last range we saw that has the largest end point, which is not the
        // standard sorting of intervals.
        // We want the largest end point interval to handle cases where there is one large interval
        // in a list that overlaps with several distinct intervals in the other list.
        let mut pending: Option<VersionRange> = None;

        loop {
            let smaller = match (this.peek(), other.peek()) {
                (Some(this_range), Some(other_range)) => {
                    if this_range < other_range {
                        this.next().expect("just peeked")
                    } else {
                        other.next().expect("just peeked")
                    }
                }
                (Some(_), None) => this.next().expect("just peeked"),
                (None, Some(_)) => other.next().expect("just peeked"),
                (None, None) => break,
            };

            if let Some(r) = pending {
                if let Some(intersect) = r.intersect(smaller) {
                    // we know that within an VersionRange, there are no overlaps, so as soon as we
                    // have an intersection, it can be pushed to the result and no other ranges
                    // will overlap with the intersection
                    out.insert(intersect);

                    // get the largest ending range
                    pending = Some(cmp::max_by(r, smaller.dupe(), |r1, r2| {
                        match (r1.end, r2.end) {
                            (None, None) => cmp::Ordering::Equal,
                            (None, Some(_)) => cmp::Ordering::Greater,
                            (Some(_), None) => cmp::Ordering::Less,
                            (Some(end), Some(other_end)) => end.cmp(&other_end),
                        }
                    }));
                } else {
                    // if there's no overlap, the current interval must have a larger ending point
                    // than `pending`, since `pending` < `smaller` and they don't intersect
                    pending = Some(smaller.dupe());
                }
            } else {
                pending = Some(smaller.dupe());
            }
        }

        VersionRanges(out)
    }

    /// Computes the intersection of this set of ranges and a range.
    pub(crate) fn intersect_range(&self, range: &VersionRange) -> VersionRanges {
        let mut ranges = VersionRanges::new();
        ranges.insert(range.dupe());
        self.intersect(&ranges)
    }

    pub(crate) fn ranges(&self) -> &BTreeSet<VersionRange> {
        &self.0
    }

    pub(crate) fn is_empty(&self) -> bool {
        // Ranges in the set are not empty, so self is not empty if the ranges set is not empty.
        self.ranges().is_empty()
    }
}

// A bit of a weird type to put the MinorVersion in Arc, but we do it to
// have weak guards.
#[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_arc_on_dupe))]
pub(crate) struct MinorVersionGuard(Arc<MinorVersion>);
pub(crate) struct MinorVersionWeak(Weak<MinorVersion>);

impl MinorVersionGuard {
    #[cfg(test)]
    pub(crate) fn testing_new(m_v: usize) -> Self {
        Self(Arc::new(MinorVersion(m_v)))
    }

    pub(crate) fn downgrade(&self) -> MinorVersionWeak {
        MinorVersionWeak(Arc::downgrade(&self.0))
    }
}

impl Deref for MinorVersionGuard {
    type Target = MinorVersion;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl MinorVersionWeak {
    pub(crate) fn new() -> MinorVersionWeak {
        MinorVersionWeak(Weak::new())
    }

    pub(crate) fn upgrade(&self) -> Option<MinorVersionGuard> {
        self.0.upgrade().map(MinorVersionGuard)
    }
}

/// The version to write to if updating the computations. This version is only
/// queried from the 'VersionTracker' when the underlying version is requested,
/// and only committed when this value is dropped.
pub(crate) struct VersionForWrites {
    v: OnceCell<VersionWriteGuard>,
    version_tracker: Arc<VersionTracker>,
}

impl VersionForWrites {
    fn new(version_tracker: Arc<VersionTracker>) -> Self {
        Self {
            v: OnceCell::new(),
            version_tracker,
        }
    }

    /// actually gets the version number to use for updates.
    pub(crate) fn get(&self) -> VersionNumber {
        let v = self.v.get_or_init(|| self.version_tracker.next());
        v.v
    }

    #[cfg(test)]
    pub(crate) fn testing_new(v: VersionNumber) -> Self {
        let lock = Arc::new(RawMutex::INIT);
        lock.lock();

        Self {
            v: OnceCell::from(VersionWriteGuard { lock, v }),
            version_tracker: VersionTracker::new(),
        }
    }
}

struct VersionWriteGuard {
    lock: Arc<RawMutex>,
    v: VersionNumber,
}

impl Drop for VersionWriteGuard {
    fn drop(&mut self) {
        unsafe { self.lock.unlock() };
    }
}

unsafe impl Send for VersionWriteGuard {}
unsafe impl Sync for VersionWriteGuard {}

impl Drop for VersionForWrites {
    fn drop(&mut self) {
        self.v
            .get()
            .iter()
            .for_each(|v| self.version_tracker.update(&v.v))
    }
}

/// Tracks the currently in-flight versions for updates and reads to ensure
/// values are up to date.
pub(crate) struct VersionTracker {
    current: RwLock<VersionToMinor>,
    /// use a RawMutex here so that we can lock and unlock using our custom `VersionWriteGuard`
    /// that is Send and Sync, so that the write version can be sent across multiple threads for
    /// Dice updates, while guaranteeing that the lock is held so that updates to dice are mutually
    /// exclusive.
    write_lock: Arc<RawMutex>,
    /// locked by write_lock above
    write_version: UnsafeCell<VersionNumber>,
}

unsafe impl Send for VersionTracker {}
unsafe impl Sync for VersionTracker {}

struct VersionToMinor {
    version: VersionNumber,
    /// index of the vec is the version, which index into the corresponding minor version tracker
    minor_version_tracker: Vec<MinorVersionTracker>,
}

impl VersionTracker {
    pub(crate) fn new() -> Arc<Self> {
        Arc::new(VersionTracker {
            current: RwLock::new(VersionToMinor {
                version: VersionNumber::ZERO,
                minor_version_tracker: vec![MinorVersionTracker::new()],
            }),
            write_lock: Arc::new(RawMutex::INIT),
            write_version: UnsafeCell::new(VersionNumber::ZERO),
        })
    }

    /// request an increase in the global version number. This returns a
    /// 'VersionNumber' that holds the next available version number. Note
    /// that the new version isn't committed to be the new global current
    /// version until the 'VersionNumber' is dropped.
    fn next(&self) -> VersionWriteGuard {
        self.write_lock.lock();
        let v = unsafe { &mut *self.write_version.get() };
        v.inc();

        VersionWriteGuard {
            lock: self.write_lock.dupe(),
            v: *v,
        }
    }

    /// hands out the current "latest" committed version and its corresponding
    /// minor version. The "latest" version is the most recent version number that was given
    /// via `next`, not the most recently committed version.
    ///
    /// the minor version is updated such that it is incremented per major version, only when
    /// there are no active owners of the minor version.
    pub(crate) fn current(&self) -> (VersionNumber, MinorVersionGuard) {
        let cur = self.current.read().unwrap();
        let v = cur.version;
        let m = cur.minor_version_tracker[v.0].acquire();
        (v, m)
    }

    /// Requests the 'WriteVersion' that is intended to be used for updates to
    /// the incremental computations
    pub(crate) fn write(self: &Arc<Self>) -> VersionForWrites {
        VersionForWrites::new(self.dupe())
    }

    /// updates the current version to the latest of the currently stored
    /// version and the given
    fn update(&self, v: &VersionNumber) {
        let mut cur = self.current.write().unwrap();

        if cur.version < *v {
            cur.minor_version_tracker
                .resize_with(v.0 + 1, MinorVersionTracker::new);
            cur.version = *v;
        }
    }
}

struct MinorVersionTracker {
    m_v: RwLock<(MinorVersionWeak, MinorVersion)>,
}

impl MinorVersionTracker {
    fn new() -> Self {
        Self {
            m_v: RwLock::new((MinorVersionWeak::new(), MinorVersion::ZERO)),
        }
    }

    fn acquire(&self) -> MinorVersionGuard {
        if let Some(m_v) = self.m_v.read().unwrap().0.upgrade() {
            return m_v;
        }
        let mut v = self.m_v.write().unwrap();
        if let Some(m_v) = v.0.upgrade() {
            m_v
        } else {
            let new_mv = MinorVersionGuard(Arc::new(v.1));
            let new_v = v.1.next();
            *v = (new_mv.downgrade(), new_v);
            new_mv
        }
    }
}

mod introspection {
    use crate::incremental::versions::VersionNumber;

    impl VersionNumber {
        pub fn to_introspectable(&self) -> crate::introspection::graph::VersionNumber {
            crate::introspection::graph::VersionNumber(self.0)
        }
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use std::collections::BTreeSet;

    use crate::incremental::versions::VersionRange;
    use crate::incremental::versions::VersionRanges;

    pub(crate) trait VersionRangesExt {
        fn testing_new(ranges: BTreeSet<VersionRange>) -> Self;
    }

    impl VersionRangesExt for VersionRanges {
        fn testing_new(ranges: BTreeSet<VersionRange>) -> Self {
            Self(ranges)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::mem;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;
    use std::sync::Barrier;
    use std::thread;
    use std::time::Duration;

    use gazebo::dupe::Dupe;

    use crate::incremental::versions::MinorVersion;
    use crate::incremental::versions::MinorVersionTracker;
    use crate::incremental::versions::VersionNumber;
    use crate::incremental::versions::VersionRange;
    use crate::incremental::versions::VersionRanges;
    use crate::incremental::versions::VersionTracker;

    #[test]
    fn simple_version_increases() {
        let vt = VersionTracker::new();
        let (v, mv) = vt.current();
        assert_eq!(
            (v, *mv),
            (VersionNumber::new(0), MinorVersion::testing_new(0))
        );

        vt.update(&vt.next().v);

        let (v, mv) = vt.current();
        assert_eq!(
            (v, *mv),
            (VersionNumber::new(1), MinorVersion::testing_new(0))
        );
    }

    #[test]
    fn write_version_commits_on_drop() {
        let vt = VersionTracker::new();
        {
            let (v, mv) = vt.current();
            assert_eq!(
                (v, *mv),
                (VersionNumber::new(0), MinorVersion::testing_new(0))
            );
        }

        {
            let v1 = vt.write();
            assert_eq!(v1.get(), VersionNumber::new(1));
            let (v, mv) = vt.current();
            assert_eq!(
                (v, *mv),
                (VersionNumber::new(0), MinorVersion::testing_new(1))
            );

            std::mem::drop(mv);

            let (v, mv) = vt.current();
            assert_eq!(
                (v, *mv),
                (VersionNumber::new(0), MinorVersion::testing_new(2))
            );

            std::mem::drop(v1);

            let v2 = vt.write();
            assert_eq!(v2.get(), VersionNumber::new(2),);
            let (v, mv) = vt.current();
            assert_eq!(
                (v, *mv),
                (VersionNumber::new(1), MinorVersion::testing_new(0))
            );
            std::mem::drop(v2);

            let (v, mv) = vt.current();
            assert_eq!(
                (v, *mv),
                (VersionNumber::new(2), MinorVersion::testing_new(0))
            );
        }
        {
            let (v, mv) = vt.current();
            assert_eq!(
                (v, *mv),
                (VersionNumber::new(2), MinorVersion::testing_new(1))
            );
        }
    }

    #[test]
    fn write_version_is_lazy() {
        let vt = VersionTracker::new();

        let write1 = vt.write();
        let write2 = vt.write();

        assert_eq!(write1.v.get().is_some(), false);
        assert_eq!(write2.v.get().is_some(), false);

        // getting write2 first gives it the lower number
        assert_eq!(write2.get(), VersionNumber::new(1));
        mem::drop(write2);

        assert_eq!(write1.get(), VersionNumber::new(2));
    }

    #[test]
    fn minor_version_updates_only_when_no_refs() {
        let vt = MinorVersionTracker::new();

        {
            let m1 = vt.acquire();
            assert_eq!(*m1, MinorVersion::testing_new(0));

            let m2 = vt.acquire();
            assert_eq!(*m2, MinorVersion::testing_new(0));

            mem::drop(m1);
            let m3 = vt.acquire();
            assert_eq!(*m3, MinorVersion::testing_new(0));
        }

        let m = vt.acquire();
        assert_eq!(*m, MinorVersion::testing_new(1));
    }

    #[test]
    fn version_range_intersects() {
        let r1 = VersionRange::bounded(VersionNumber::new(0), VersionNumber::new(4));
        let r2 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(5));
        assert_eq!(
            r1.intersect(&r2),
            Some(VersionRange::bounded(
                VersionNumber::new(1),
                VersionNumber::new(4)
            ))
        );

        let r1 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4));
        let r2 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(5));
        assert_eq!(
            r1.intersect(&r2),
            Some(VersionRange::bounded(
                VersionNumber::new(1),
                VersionNumber::new(4)
            ))
        );

        let r1 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4));
        let r2 = VersionRange::begins_with(VersionNumber::new(0));
        assert_eq!(
            r1.intersect(&r2),
            Some(VersionRange::bounded(
                VersionNumber::new(1),
                VersionNumber::new(4)
            ))
        );

        let r1 = VersionRange::begins_with(VersionNumber::new(2));
        let r2 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(5));
        assert_eq!(
            r1.intersect(&r2),
            Some(VersionRange::bounded(
                VersionNumber::new(2),
                VersionNumber::new(5)
            ))
        );

        let r1 = VersionRange::begins_with(VersionNumber::new(2));
        let r2 = VersionRange::begins_with(VersionNumber::new(1));
        assert_eq!(
            r1.intersect(&r2),
            Some(VersionRange::begins_with(VersionNumber::new(2)))
        );

        let r1 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(2));
        let r2 = VersionRange::bounded(VersionNumber::new(3), VersionNumber::new(4));
        assert_eq!(r1.intersect(&r2), None,);
    }

    #[test]
    fn version_range_splits() {
        let mut r1 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4));
        assert_eq!(r1.split(VersionNumber::new(0)), None);
        assert_eq!(
            r1,
            VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4))
        );

        let mut r1 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4));
        assert_eq!(r1.split(VersionNumber::new(5)), None,);
        assert_eq!(
            r1,
            VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4))
        );

        let mut r1 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4));
        assert_eq!(
            r1.split(VersionNumber::new(3)),
            Some(VersionRange::bounded(
                VersionNumber::new(3),
                VersionNumber::new(4)
            )),
        );
        assert_eq!(
            r1,
            VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(3))
        );

        let mut r1 = VersionRange::begins_with(VersionNumber::new(2));
        assert_eq!(r1.split(VersionNumber::new(1)), None);
        assert_eq!(r1, VersionRange::begins_with(VersionNumber::new(2)));

        let mut r1 = VersionRange::begins_with(VersionNumber::new(2));
        assert_eq!(
            r1.split(VersionNumber::new(4)),
            Some(VersionRange::begins_with(VersionNumber::new(4)))
        );
        assert_eq!(
            r1,
            VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(4))
        );
    }

    #[test]
    fn version_range_ops() {
        let r1 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4));
        let r2 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4));

        assert_eq!(r1 == r2, true);
        assert_eq!(r1 < r2, false);
        assert_eq!(r1 > r2, false);

        let r2 = VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(5));
        assert_eq!(r1 == r2, false);
        assert_eq!(r1 < r2, true);
        assert_eq!(r1 > r2, false);

        let r2 = VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(3));
        assert_eq!(r1 == r2, false);
        assert_eq!(r1 < r2, true);
        assert_eq!(r1 > r2, false);

        let r2 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(3));
        assert_eq!(r1 == r2, false);
        assert_eq!(r1 < r2, false);
        assert_eq!(r1 > r2, true);

        let r2 = VersionRange::begins_with(VersionNumber::new(2));
        assert_eq!(r1 == r2, false);
        assert_eq!(r1 < r2, true);
        assert_eq!(r1 > r2, false);

        let r2 = VersionRange::begins_with(VersionNumber::new(0));
        assert_eq!(r1 == r2, false);
        assert_eq!(r1 < r2, false);
        assert_eq!(r1 > r2, true);

        let r1 = VersionRange::begins_with(VersionNumber::new(1));
        let r2 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4));
        assert_eq!(r1 == r2, false);
        assert_eq!(r1 < r2, false);
        assert_eq!(r1 > r2, true);

        let r1 = VersionRange::begins_with(VersionNumber::new(1));
        let r2 = VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(4));
        assert_eq!(r1 == r2, false);
        assert_eq!(r1 < r2, true);
        assert_eq!(r1 > r2, false);

        let r1 = VersionRange::begins_with(VersionNumber::new(1));
        let r2 = VersionRange::begins_with(VersionNumber::new(1));
        assert_eq!(r1 == r2, true);
        assert_eq!(r1 < r2, false);
        assert_eq!(r1 > r2, false);
    }

    #[test]
    fn version_range_merge() {
        let r1 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4));
        let r2 = VersionRange::bounded(VersionNumber::new(5), VersionNumber::new(6));

        assert_eq!(r1.merge(&r2), None);

        let r2 = VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(3));
        assert_eq!(
            r1.merge(&r2),
            Some(VersionRange::bounded(
                VersionNumber::new(1),
                VersionNumber::new(4)
            ))
        );

        let r2 = VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(6));
        assert_eq!(
            r1.merge(&r2),
            Some(VersionRange::bounded(
                VersionNumber::new(1),
                VersionNumber::new(6)
            ))
        );

        let r2 = VersionRange::begins_with(VersionNumber::new(6));
        assert_eq!(r1.merge(&r2), None);

        let r2 = VersionRange::begins_with(VersionNumber::new(2));
        assert_eq!(
            r1.merge(&r2),
            Some(VersionRange::begins_with(VersionNumber::new(1),))
        );
    }

    #[test]
    fn version_ranges_union() {
        let r1 = VersionRanges(btreeset![
            VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(3)),
            VersionRange::bounded(VersionNumber::new(6), VersionNumber::new(8)),
            VersionRange::bounded(VersionNumber::new(10), VersionNumber::new(11))
        ]);

        let r2 = VersionRanges(btreeset![
            VersionRange::bounded(VersionNumber::new(0), VersionNumber::new(1)),
            VersionRange::bounded(VersionNumber::new(4), VersionNumber::new(5)),
            VersionRange::bounded(VersionNumber::new(8), VersionNumber::new(10)),
            VersionRange::bounded(VersionNumber::new(11), VersionNumber::new(12)),
            VersionRange::begins_with(VersionNumber::new(13)),
        ]);

        assert_eq!(
            r1.union(&r2),
            VersionRanges(btreeset![
                VersionRange::bounded(VersionNumber::new(0), VersionNumber::new(3)),
                VersionRange::bounded(VersionNumber::new(4), VersionNumber::new(5)),
                VersionRange::bounded(VersionNumber::new(6), VersionNumber::new(12)),
                VersionRange::begins_with(VersionNumber::new(13)),
            ])
        );
    }

    #[test]
    fn version_ranges_intersect() {
        let r1 = VersionRanges(btreeset![VersionRange::bounded(
            VersionNumber::new(1),
            VersionNumber::new(3)
        ),]);

        let r2 = VersionRanges(btreeset![
            VersionRange::bounded(VersionNumber::new(4), VersionNumber::new(5)),
            VersionRange::bounded(VersionNumber::new(11), VersionNumber::new(12)),
            VersionRange::begins_with(VersionNumber::new(13)),
        ]);

        assert_eq!(r1.intersect(&r2), VersionRanges::new());

        let r1 = VersionRanges(btreeset![
            VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(3)),
            VersionRange::bounded(VersionNumber::new(4), VersionNumber::new(5)),
            VersionRange::bounded(VersionNumber::new(6), VersionNumber::new(9)),
            VersionRange::bounded(VersionNumber::new(10), VersionNumber::new(14)),
            VersionRange::begins_with(VersionNumber::new(15)),
        ]);

        let r2 = VersionRanges(btreeset![
            VersionRange::bounded(VersionNumber::new(0), VersionNumber::new(1)),
            VersionRange::bounded(VersionNumber::new(4), VersionNumber::new(5)),
            VersionRange::bounded(VersionNumber::new(8), VersionNumber::new(10)),
            VersionRange::bounded(VersionNumber::new(11), VersionNumber::new(12)),
            VersionRange::begins_with(VersionNumber::new(13)),
        ]);

        assert_eq!(
            r1.intersect(&r2),
            VersionRanges(btreeset![
                VersionRange::bounded(VersionNumber::new(4), VersionNumber::new(5)),
                VersionRange::bounded(VersionNumber::new(8), VersionNumber::new(9)),
                VersionRange::bounded(VersionNumber::new(11), VersionNumber::new(12)),
                VersionRange::bounded(VersionNumber::new(13), VersionNumber::new(14)),
                VersionRange::begins_with(VersionNumber::new(15)),
            ])
        );
    }

    #[test]
    fn version_ranges_intersects_range() {
        let r1 = VersionRanges(btreeset![
            VersionRange::bounded(VersionNumber(10), VersionNumber(20)),
            VersionRange::bounded(VersionNumber(30), VersionNumber(40)),
        ]);
        let r2 = VersionRange::bounded(VersionNumber(15), VersionNumber(35));
        let expected = VersionRanges(btreeset![
            VersionRange::bounded(VersionNumber(15), VersionNumber(20)),
            VersionRange::bounded(VersionNumber(30), VersionNumber(35)),
        ]);
        assert_eq!(expected, r1.intersect_range(&r2));
    }

    #[test]
    fn version_ranges_insert() {
        let mut r = VersionRanges::new();

        r.insert(VersionRange::bounded(
            VersionNumber::new(1),
            VersionNumber::new(3),
        ));
        assert_eq!(
            r.ranges(),
            &btreeset![VersionRange::bounded(
                VersionNumber::new(1),
                VersionNumber::new(3)
            )]
        );

        r.insert(VersionRange::bounded(
            VersionNumber::new(4),
            VersionNumber::new(6),
        ));
        assert_eq!(
            r.ranges(),
            &btreeset![
                VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(3)),
                VersionRange::bounded(VersionNumber::new(4), VersionNumber::new(6))
            ]
        );

        r.insert(VersionRange::bounded(
            VersionNumber::new(5),
            VersionNumber::new(7),
        ));
        assert_eq!(
            r.ranges(),
            &btreeset![
                VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(3)),
                VersionRange::bounded(VersionNumber::new(4), VersionNumber::new(7))
            ]
        );

        r.insert(VersionRange::bounded(
            VersionNumber::new(0),
            VersionNumber::new(1),
        ));
        assert_eq!(
            r.ranges(),
            &btreeset![
                VersionRange::bounded(VersionNumber::new(0), VersionNumber::new(3)),
                VersionRange::bounded(VersionNumber::new(4), VersionNumber::new(7))
            ]
        );
    }

    #[test]
    fn version_write_is_exclusive() {
        let tracker = VersionTracker::new();
        let write_v = tracker.write();
        assert_eq!(write_v.get(), VersionNumber::new(1));

        let barrier = Arc::new(Barrier::new(2));

        let is_ran = Arc::new(AtomicBool::new(false));

        let handle = thread::spawn({
            let tracker = tracker.dupe();
            let is_ran = is_ran.dupe();
            let barrier = barrier.dupe();
            move || {
                barrier.wait();

                let write_v = tracker.write();
                assert_eq!(write_v.get(), VersionNumber::new(2));

                is_ran.store(true, Ordering::SeqCst);
            }
        });

        barrier.wait();
        // sadly there's still a race that the spawned thread might not execute the call to `write()`
        // so we just sleep here briefly and hope.
        thread::sleep(Duration::from_secs(1));

        assert_eq!(is_ran.load(Ordering::SeqCst), false);

        mem::drop(write_v);

        handle.join().unwrap();
    }
}
