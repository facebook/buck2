/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Deref;
use std::ops::RangeFrom;
use std::ops::RangeTo;
use std::str::CharIndices;
use std::str::Chars;

use gazebo::dupe::Dupe;
use nom::Compare;
use nom::CompareResult;
use nom::InputIter;
use nom::InputLength;
use nom::InputTake;
use nom::Needed;
use nom::Offset;
use nom::Slice;
use nom::UnspecializedInput;

/// Similar to `LocatedSpan` from `nom_locate`, but without line number.
#[derive(Clone, Debug, PartialEq, Copy, Dupe)]
pub struct Span<'a> {
    offset: usize,
    fragment: &'a str,
}

impl<'a> Deref for Span<'a> {
    type Target = str;

    fn deref(&self) -> &str {
        self.fragment
    }
}

impl<'a> Span<'a> {
    pub(crate) fn new(s: &'a str) -> Span<'a> {
        Span {
            offset: 0,
            fragment: s,
        }
    }

    pub fn fragment(&self) -> &'a str {
        self.fragment
    }

    pub(crate) fn location_offset(&self) -> usize {
        self.offset
    }
}

impl<'a> UnspecializedInput for Span<'a> {}

impl<'a> InputLength for Span<'a> {
    fn input_len(&self) -> usize {
        self.fragment.input_len()
    }
}

impl<'a> Slice<RangeFrom<usize>> for Span<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        Span {
            offset: self.offset + range.start,
            fragment: &self.fragment[range.start..],
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Span<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Span {
            offset: self.offset,
            fragment: &self.fragment[..range.end],
        }
    }
}

impl<'a> InputTake for Span<'a> {
    fn take(&self, count: usize) -> Self {
        Span {
            offset: self.offset,
            fragment: &self.fragment[..count],
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (a, b) = self.fragment.split_at(count);
        (
            Span {
                offset: self.offset + count,
                fragment: b,
            },
            Span {
                offset: self.offset,
                fragment: a,
            },
        )
    }
}

impl<'a> InputIter for Span<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;

    fn iter_indices(&self) -> Self::Iter {
        self.fragment.char_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.fragment.chars()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.fragment.position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        self.fragment.slice_index(count)
    }
}

impl<'a> Offset for Span<'a> {
    fn offset(&self, second: &Self) -> usize {
        let fst = self.offset;
        let snd = second.offset;

        snd - fst
    }
}

impl<'a> Compare<&'a str> for Span<'a> {
    fn compare(&self, t: &'a str) -> CompareResult {
        self.fragment.compare(t)
    }

    fn compare_no_case(&self, t: &'a str) -> CompareResult {
        self.fragment.compare_no_case(t)
    }
}
