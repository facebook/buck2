extern crate quickcheck;

use quickcheck::{quickcheck, TestResult};
use std::collections::BTreeSet;
use std::ops::Bound::{self, *};

/// Covers every `std::ops::Range*` plus variants with exclusive start.
type RangeAny<T> = (Bound<T>, Bound<T>);

/// Mimic `RangeBounds::contains`, stabilized in Rust 1.35.
trait RangeBounds<T> {
    fn contains(&self, _: &T) -> bool;
}
impl<T: PartialOrd> RangeBounds<T> for RangeAny<T> {
    fn contains(&self, item: &T) -> bool {
        (match &self.0 {
            Included(start) => start <= item,
            Excluded(start) => start < item,
            Unbounded => true,
        }) && (match &self.1 {
            Included(end) => item <= end,
            Excluded(end) => item < end,
            Unbounded => true,
        })
    }
}

/// Checks conditions where `BTreeSet::range` panics:
/// - Panics if range start > end.
/// - Panics if range start == end and both bounds are Excluded.
fn panics<T: PartialOrd>(range: RangeAny<T>) -> bool {
    match (&range.0, &range.1) {
        (Excluded(start), Excluded(end)) => start >= end,
        (Included(start), Excluded(end))
        | (Excluded(start), Included(end))
        | (Included(start), Included(end)) => start > end,
        (Unbounded, _) | (_, Unbounded) => false,
    }
}

/// Checks that `BTreeSet::range` returns all items contained in the given `range`.
fn check_range(set: BTreeSet<i32>, range: RangeAny<i32>) -> TestResult {
    if panics(range) {
        TestResult::discard()
    } else {
        let xs: BTreeSet<_> = set.range(range).cloned().collect();
        TestResult::from_bool(
            set.iter().all(|x| range.contains(x) == xs.contains(x)),
        )
    }
}

fn main() {
    quickcheck(check_range as fn(_, _) -> TestResult);
}
