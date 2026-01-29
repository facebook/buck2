/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//!
//! This library contains utilities for tracking a global version number. The
//! global version number is used for tagging computed values so that we can
//! track when a value needs to be updated because its version number is out of
//! date.

use std::cmp;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::ops::Bound;
use std::ops::RangeBounds;
use std::ops::Sub;

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;

/// The incrementing Version number associated with all the cache entries
#[derive(Copy, Eq, Debug, Display, Dupe)]
// split this due to formatters not agreeing
#[derive(PartialEq, Hash, Clone, Ord, PartialOrd, Allocative)]
#[display("v{}", _0)]
pub struct VersionNumber(pub(crate) usize);

impl VersionNumber {
    /// First transaction has version number zero.
    pub(crate) const ZERO: VersionNumber = VersionNumber(0);

    pub(crate) fn new(num: usize) -> Self {
        VersionNumber(num)
    }

    pub(crate) fn inc(&mut self) {
        self.0 += 1;
    }

    pub(crate) fn dec(&mut self) {
        self.0 = self.0.checked_sub(1).expect("shouldn't underflow");
    }

    pub fn value(&self) -> usize {
        self.0
    }
}

impl Sub for VersionNumber {
    type Output = isize;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0 as isize - rhs.0 as isize
    }
}

mod introspection {
    use crate::versions::VersionNumber;

    impl VersionNumber {
        pub fn to_introspectable(&self) -> crate::introspection::graph::VersionNumber {
            crate::introspection::graph::VersionNumber(self.0)
        }
    }
}

/// Represents a range of versions. This range must have a start that is inclusive, but may be
/// unbounded towards the end. The end, if present, is exclusive.
#[derive(Allocative, Eq, Debug, Dupe, PartialEq, Hash, Clone, Copy)]
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
                write!(f, "{end}")?;
            }
        }
        write!(f, ")")
    }
}

impl VersionRange {
    pub(crate) fn new(begin: VersionNumber, end: Option<VersionNumber>) -> Self {
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

    #[allow(unused)] // TODO(cjhopman): This will be used.
    pub(crate) fn into_ranges(self) -> VersionRanges {
        VersionRanges(vec![self])
    }

    pub(crate) fn intersect(&self, other: &VersionRange) -> Option<Self> {
        // we exclude the end bound, because intervals [1,2) and [2,3) do not intersect
        fn contains_end_exclusive(
            v: VersionNumber,
            begin: VersionNumber,
            end: Option<VersionNumber>,
        ) -> bool {
            v >= begin && end.is_none_or(|end| v < end)
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
    pub(crate) fn begin(&self) -> VersionNumber {
        self.begin
    }

    #[allow(unused)] // useful function
    pub(crate) fn end(&self) -> Option<VersionNumber> {
        self.end
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
            v >= begin && end.is_none_or(|end| v <= end)
        }

        if is_between_end_inclusive(self.begin, other.begin, other.end)
            || is_between_end_inclusive(other.begin, self.begin, self.end)
        {
            Some(self.merge_unchecked(other))
        } else {
            None
        }
    }

    /// Merges this range with the given range assuming that they overlap
    fn merge_unchecked(&self, other: &VersionRange) -> VersionRange {
        VersionRange::new(
            cmp::min(self.begin, other.begin),
            match (self.end, other.end) {
                (None, _) => None,
                (_, None) => None,
                (Some(e1), Some(e2)) => Some(cmp::max(e1, e2)),
            },
        )
    }
}

// TODO(cjhopman): While implementing RangeBounds gives access to a bunch of apis, they are all kinda deceptive
// because VersionRange bounds are more restricted than RangeBounds are and so using many of the apis is kinda awkward.
impl RangeBounds<VersionNumber> for VersionRange {
    fn start_bound(&self) -> Bound<&VersionNumber> {
        Bound::Included(&self.begin)
    }

    fn end_bound(&self) -> Bound<&VersionNumber> {
        self.end.as_ref().map_or(Bound::Unbounded, Bound::Excluded)
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
#[derive(Allocative, Eq, Debug, PartialEq, Hash, Clone, PartialOrd, Ord)]
pub(crate) struct VersionRanges(Vec<VersionRange>);

impl Display for VersionRanges {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (i, range) in self.0.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{range}")?;
        }
        write!(f, "}}")
    }
}

impl VersionRanges {
    /// Returns the last range if this is non-empty.
    pub(crate) fn last(&self) -> Option<VersionRange> {
        self.0.last().copied()
    }

    /// Find the largest version which is at most `v`
    pub(crate) fn find_value_upper_bound(&self, v: VersionNumber) -> Option<VersionNumber> {
        // we generally expect queries at later versions so just look through the list from the
        // end. potentially this should be changed if that expectation is no longer true.
        for range in self.0.iter().rev() {
            if range.begin <= v {
                if range.contains(&v) {
                    return Some(v);
                } else {
                    let mut end = range.end.unwrap();
                    end.dec();
                    assert!(end < v);
                    return Some(end);
                }
            }
        }
        None
    }

    /// same as union_range
    pub(crate) fn insert(&mut self, range: VersionRange) {
        self.union_range(range)
    }

    /// unions with a single range, merging different ranges if necessary
    pub(crate) fn union_range(&mut self, range: VersionRange) {
        let len = self.0.len();

        // this works by finding a position to insert the new range, and then unions it with any overlapping ranges after its insertion point.

        let idx = self.0.partition_point(|r| match r.end {
            Some(end) if end < range.begin => true,
            _ => false,
        });

        // idx now points to the first range with end >= range.begin, which is the position where we will "insert" the new range.
        // there's three cases:
        // 1. idx = len, we just append range at the end: there's nothing to merge
        // 2. self.0[idx].begin > range.end: there's no overlapping ranges, just insert range at idx
        // 3. self.0[idx] and range have overlap: merge them and then need to scan for and merge any additional overlap after idx

        // handle case 1
        if idx == len {
            self.0.push(range);
            return;
        }

        let mut merged = match self.0[idx].merge(&range) {
            Some(merged) => merged,
            None => {
                // no overlap, handle case 2
                self.0.insert(idx, range);
                return;
            }
        };

        let first_non_overlap = match merged.end {
            None => len,
            Some(end) => (idx + 1) + self.0[(idx + 1)..].partition_point(|r| r.begin <= end),
        };

        let last_overlap = first_non_overlap - 1;
        if last_overlap > idx {
            // the inserted range overlaps multiple entries, we need to use the largest end value of all the overlapped
            // ranges (which is either the end of the last one or the end of the one we're inserting).
            merged = merged.merge_unchecked(&self.0[last_overlap]);
        }

        self.0[idx] = merged;
        // Vec::drain is the most efficient way to remove a range.
        self.0.drain((idx + 1)..first_non_overlap);
    }

    /// Computes the union of this set of ranges and another
    #[allow(unused)] // useful function
    pub(crate) fn union(&self, other: &VersionRanges) -> VersionRanges {
        let mut this = self.0.iter().peekable();
        let mut other = other.0.iter().peekable();

        let mut out = Vec::new();
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
                        out.push(last);
                        smaller.dupe()
                    }
                },
            ));
        }

        if let Some(last) = pending {
            out.push(last);
        }

        VersionRanges(out)
    }

    #[allow(unused)] // TODO(cjhopman): This will be used.
    pub(crate) fn union_in_place(&mut self, other: &VersionRanges) {
        // TODO(cjhopman): implement this efficiently.
        *self = self.union(other);
    }

    /// Computes the intersection of this set of ranges and another
    pub(crate) fn intersect(&self, other: &VersionRanges) -> VersionRanges {
        let mut this = self.0.iter().peekable();
        let mut other = other.0.iter().peekable();

        let mut out = Vec::new();
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
                    out.push(intersect);

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

    #[allow(unused)] // TODO(cjhopman): This will be used.
    pub(crate) fn intersect_in_place(&mut self, other: &VersionRanges) {
        if self != other {
            *self = self.intersect(other)
        }
    }

    /// Computes the intersection of this set of ranges and a range.
    #[allow(unused)] // TODO(cjhopman): This will be used.
    pub(crate) fn intersect_range(&mut self, range: VersionRange) -> bool {
        if self.is_empty() {
            return false;
        }

        let self_begin = self.0.first().unwrap().begin;
        let self_end = self.0.last().unwrap().end;

        if range.begin <= self_begin {
            match (self_end, range.end) {
                (Some(_), None) => {
                    return false;
                }
                (Some(self_end), Some(range_end)) if self_end <= range_end => {
                    return false;
                }
                _ => {}
            }
        }

        if let Some(end) = range.end {
            for j in (0..self.0.len()).rev() {
                let v = &mut self.0[j];
                if v.begin < end {
                    match v.end {
                        Some(this_end) if this_end < end => {}
                        _ => v.end = Some(end),
                    }
                    break;
                } else {
                    self.0.pop();
                }
            }
        }

        let begin = range.begin;
        let mut i = 0;
        while i < self.0.len() {
            let v = &mut self.0[i];

            match v.end {
                Some(e) if e <= begin => {
                    i += 1;
                    continue;
                }
                _ => {}
            };

            if v.begin < begin {
                v.begin = begin;
            }
            break;
        }
        if i < self.0.len() {
            self.0.drain(0..i);
        } else {
            self.clear()
        }

        true
    }

    pub(crate) fn is_empty(&self) -> bool {
        // Ranges in the set are not empty, so self is not empty if the ranges set is not empty.
        self.0.is_empty()
    }

    pub(crate) fn contains(&self, version: VersionNumber) -> bool {
        self.find_value_upper_bound(version) == Some(version)
    }

    pub(crate) fn to_introspectable(
        &self,
    ) -> Vec<(
        crate::introspection::graph::VersionNumber,
        Option<crate::introspection::graph::VersionNumber>,
    )> {
        self.0
            .iter()
            .map(|v| {
                (
                    v.begin().to_introspectable(),
                    v.end().map(|v| v.to_introspectable()),
                )
            })
            .collect()
    }

    pub(crate) fn clear(&mut self) {
        self.0.clear()
    }
}

#[cfg(test)]
impl VersionRanges {
    pub(crate) fn new() -> Self {
        Self(Default::default())
    }

    pub(crate) fn testing_new(ranges: Vec<VersionRange>) -> Self {
        Self(ranges)
    }
}

#[cfg(test)]
mod tests {
    use std::ops::RangeBounds;

    use crate::versions::VersionNumber;
    use crate::versions::VersionRange;
    use crate::versions::VersionRanges;

    #[track_caller]
    fn into_range(range: (i32, i32)) -> VersionRange {
        let (b, e) = range;
        match e {
            -1 => VersionRange::begins_with(VersionNumber::new(b.try_into().unwrap())),
            e => VersionRange::bounded(
                VersionNumber::new(b.try_into().unwrap()),
                VersionNumber::new(e.try_into().unwrap()),
            ),
        }
    }

    #[track_caller]
    fn into_ranges<const N: usize>(ranges: [(i32, i32); N]) -> VersionRanges {
        VersionRanges(ranges.iter().copied().map(into_range).collect())
    }

    #[test]
    fn version_range_contains() {
        let r1 = VersionRange::bounded(VersionNumber::new(3), VersionNumber::new(6));
        assert!(!(r1.contains(&VersionNumber::new(1))));
        assert!(!(r1.contains(&VersionNumber::new(2))));
        assert!(r1.contains(&VersionNumber::new(3)));
        assert!(r1.contains(&VersionNumber::new(4)));
        assert!(r1.contains(&VersionNumber::new(5)));
        assert!(!(r1.contains(&VersionNumber::new(6))));
        assert!(!(r1.contains(&VersionNumber::new(7))));
        assert!(!(r1.contains(&VersionNumber::new(8))));

        let r1 = VersionRange::begins_with(VersionNumber::new(3));
        assert!(!(r1.contains(&VersionNumber::new(2))));
        assert!(r1.contains(&VersionNumber::new(3)));
        assert!(r1.contains(&VersionNumber::new(4)));
        assert!(r1.contains(&VersionNumber::new(5000)));
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
        assert_eq!(r1.intersect(&r2), None);
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
        assert_eq!(r1.split(VersionNumber::new(5)), None);
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

        assert!(r1 == r2);
        assert!(!(r1 < r2));
        assert!(!(r1 > r2));

        let r2 = VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(5));
        assert!(!(r1 == r2));
        assert!(r1 < r2);
        assert!(!(r1 > r2));

        let r2 = VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(3));
        assert!(!(r1 == r2));
        assert!(r1 < r2);
        assert!(!(r1 > r2));

        let r2 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(3));
        assert!(!(r1 == r2));
        assert!(!(r1 < r2));
        assert!(r1 > r2);

        let r2 = VersionRange::begins_with(VersionNumber::new(2));
        assert!(!(r1 == r2));
        assert!(r1 < r2);
        assert!(!(r1 > r2));

        let r2 = VersionRange::begins_with(VersionNumber::new(0));
        assert!(!(r1 == r2));
        assert!(!(r1 < r2));
        assert!(r1 > r2);

        let r1 = VersionRange::begins_with(VersionNumber::new(1));
        let r2 = VersionRange::bounded(VersionNumber::new(1), VersionNumber::new(4));
        assert!(!(r1 == r2));
        assert!(!(r1 < r2));
        assert!(r1 > r2);

        let r1 = VersionRange::begins_with(VersionNumber::new(1));
        let r2 = VersionRange::bounded(VersionNumber::new(2), VersionNumber::new(4));
        assert!(!(r1 == r2));
        assert!(r1 < r2);
        assert!(!(r1 > r2));

        let r1 = VersionRange::begins_with(VersionNumber::new(1));
        let r2 = VersionRange::begins_with(VersionNumber::new(1));
        assert!(r1 == r2);
        assert!(!(r1 < r2));
        assert!(!(r1 > r2));
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
        let r1 = into_ranges([(1, 3), (6, 8), (10, 11)]);
        let r2 = into_ranges([(0, 1), (4, 5), (8, 10), (11, 12), (13, -1)]);

        assert_eq!(
            r1.union(&r2),
            into_ranges([(0, 3), (4, 5), (6, 12), (13, -1)])
        );
    }

    #[test]
    fn version_ranges_intersect() {
        let r1 = into_ranges([(1, 3)]);
        let r2 = into_ranges([(4, 5), (11, 12), (13, -1)]);

        assert_eq!(r1.intersect(&r2), VersionRanges::new());

        let r1 = into_ranges([(1, 3), (4, 5), (6, 9), (10, 14), (15, -1)]);
        let r2 = into_ranges([(0, 1), (4, 5), (8, 10), (11, 12), (13, -1)]);

        assert_eq!(
            r1.intersect(&r2),
            into_ranges([(4, 5), (8, 9), (11, 12), (13, 14), (15, -1)])
        );
    }

    #[test]
    fn version_ranges_intersects_range() {
        #[track_caller]
        fn assert_intersect_range<const N: usize, const M: usize>(
            initial: [(i32, i32); N],
            intersect_with: (i32, i32),
            expected: [(i32, i32); M],
        ) {
            let initial = into_ranges(initial);
            let mut as_ranges = initial.clone();
            let intersect_with = into_range(intersect_with);
            as_ranges.intersect_range(intersect_with);
            let expected_ranges = into_ranges(expected);

            assert_eq!(
                as_ranges, expected_ranges,
                "in assert_intersect_range(\n  {initial:?},\n  {intersect_with:?},\n  {expected:?}\n)"
            )
        }

        assert_intersect_range([], (0, -1), []);

        assert_intersect_range(
            [(10, 20), (30, 40), (50, 60)],
            (35, -1),
            [(35, 40), (50, 60)],
        );

        // check cases for begin (before all ranges, between ranges, at beginning, within, at end, after all)
        assert_intersect_range(
            [(10, 20), (30, 40), (50, 60)],
            (5, 55),
            [(10, 20), (30, 40), (50, 55)],
        );

        assert_intersect_range(
            [(10, 20), (30, 40), (50, 60)],
            (25, 55),
            [(30, 40), (50, 55)],
        );

        assert_intersect_range(
            [(10, 20), (30, 40), (50, 60)],
            (30, 55),
            [(30, 40), (50, 55)],
        );

        assert_intersect_range(
            [(10, 20), (30, 40), (50, 60)],
            (35, 55),
            [(35, 40), (50, 55)],
        );

        assert_intersect_range([(10, 20), (30, 40), (50, 60)], (40, 55), [(50, 55)]);

        assert_intersect_range([(10, 20), (30, 40), (50, 60)], (65, 75), []);

        // And check similar cases for end
        assert_intersect_range([(10, 20), (30, 40), (50, 60)], (0, 5), []);

        assert_intersect_range([(10, 20), (30, 40), (50, 60)], (35, 45), [(35, 40)]);

        assert_intersect_range([(10, 20), (30, 40), (50, 60)], (35, 50), [(35, 40)]);

        assert_intersect_range(
            [(10, 20), (30, 40), (50, 60)],
            (35, 55),
            [(35, 40), (50, 55)],
        );

        assert_intersect_range(
            [(10, 20), (30, 40), (50, 60)],
            (35, 60),
            [(35, 40), (50, 60)],
        );

        assert_intersect_range(
            [(10, 20), (30, 40), (50, 60)],
            (35, 65),
            [(35, 40), (50, 60)],
        );
    }

    #[test]
    fn version_ranges_insert() {
        #[track_caller]
        fn test_insert<const N: usize, const M: usize>(
            initial: [(i32, i32); N],
            range: (i32, i32),
            expected: [(i32, i32); M],
        ) {
            let initial = into_ranges(initial);
            let mut r = initial.clone();
            let range = into_range(range);
            r.insert(range);
            let expected = into_ranges(expected);
            assert!(
                r == expected,
                "test_insert assertion failed\n initial: {initial}\n range: {range}\n expected: {expected}\n actual: {r}"
            );
        }

        test_insert([], (1, 3), [(1, 3)]);

        test_insert([(1, 3)], (4, 6), [(1, 3), (4, 6)]);

        // Before: |...) |...)
        // Insert:          |...)
        test_insert([(1, 3), (4, 6)], (5, 7), [(1, 3), (4, 7)]);

        // Before:   |...) |...)
        // Insert: |...)
        test_insert([(1, 3), (4, 7)], (0, 1), [(0, 3), (4, 7)]);

        // Before: |...)   |...)   |...)
        // Insert:    |..)
        test_insert(
            [(20, 25), (30, 35), (40, 45)],
            (22, 27),
            [(20, 27), (30, 35), (40, 45)],
        );

        // Before: |...)   |...)   |...)
        // Insert:    |....)
        test_insert(
            [(20, 25), (30, 35), (40, 45)],
            (22, 30),
            [(20, 35), (40, 45)],
        );

        // Before: |...)   |...)   |...)
        // Insert:     |...)
        test_insert(
            [(20, 25), (30, 35), (40, 45)],
            (25, 30),
            [(20, 35), (40, 45)],
        );

        // Before: |...)   |...)   |...)
        // Insert:    |......)
        test_insert(
            [(20, 25), (30, 35), (40, 45)],
            (22, 33),
            [(20, 35), (40, 45)],
        );

        // Before: |...)   |...)   |...)
        // Insert:     |...........)
        test_insert([(20, 25), (30, 35), (40, 45)], (25, 40), [(20, 45)]);

        // Before: |...)   |...)   |...)
        // Insert:    |..........)
        test_insert(
            [(20, 25), (30, 35), (40, 45)],
            (22, 37),
            [(20, 37), (40, 45)],
        );

        // Before: |...)   |...)   |...)
        // Insert:    |...................)
        test_insert([(20, 25), (30, 35), (40, 45)], (22, 47), [(20, 47)]);

        // Before: |...)   |...)   |...)
        // Insert:    |...................inf
        test_insert([(20, 25), (30, 35), (40, 45)], (22, -1), [(20, -1)]);

        // Before: |...)   |...)   |...)   |...inf
        // Insert:    |.....................)
        test_insert(
            [(20, 25), (30, 35), (40, 45), (50, -1)],
            (22, 50),
            [(20, -1)],
        );

        test_insert(
            [
                (20, 25),
                (30, 35),
                (40, 45),
                (50, 55),
                (60, 65),
                (70, 75),
                (80, 85),
            ],
            (22, 42),
            [(20, 45), (50, 55), (60, 65), (70, 75), (80, 85)],
        );
    }
}
