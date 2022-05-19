/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! Our string operations (indexing) are O(n) because of our current representation.
//! There are plans afoot to change that, but in the meantime let's use fast algorithms
//! to make up some of the difference.

use std::{cmp::min, str};

use crate::{
    stdlib::util::convert_indices,
    values::types::{none::NoneOr, string::CharIndex},
};

#[inline(always)]
fn is_1byte(x: u8) -> bool {
    x & 0x80 == 0
}

#[inline(always)]
fn is_1bytes(x: u64) -> bool {
    x & 0x8080808080808080 == 0
}

/// Skip at most n 1byte characters from the prefix of the string, return how many you skipped.
/// The result will be between 0 and n.
/// The string _must_ have at least n bytes in it.
fn skip_at_most_1byte(x: &str, n: usize) -> usize {
    if n == 0 {
        return 0;
    }
    debug_assert!(x.len() >= n);

    // Multi-byte UTF8 characters have 0x80 set.
    // We first process enough characters so we align on an 8-byte boundary,
    // then process 8 bytes at a time.
    // If we see a higher value, we bail to the standard Rust code.
    // It is possible to do faster with population count, but we don't expect many real UTF8 strings.
    // (c.f. https://github.com/haskell-foundation/foundation/blob/master/foundation/cbits/foundation_utf8.c)

    // Same function, but returning the end of the string
    fn f(x: &str, n: usize) -> *const u8 {
        let leading = min(x.as_ptr().align_offset(8), n);
        let trailing = (n - leading) % 8;
        let loops = (n - leading) / 8;

        // Rather than flip between string and pointer, we stick to working with the pointer
        let mut p = x.as_ptr();

        // Loop over 1 byte at a time until we reach alignment
        for _ in 0..leading {
            if is_1byte(unsafe { *p }) {
                p = unsafe { p.add(1) };
            } else {
                return p;
            }
        }

        // Loop over 8 bytes at a time, until we reach the end
        let mut p = p as *const u64;
        for _ in 0..loops {
            if is_1bytes(unsafe { *p }) {
                p = unsafe { p.add(1) };
            } else {
                return p as *const u8;
            }
        }

        // Mop up all trailing bytes
        let mut p = p as *const u8;
        for _ in 0..trailing {
            if is_1byte(unsafe { *p }) {
                p = unsafe { p.add(1) };
            } else {
                return p;
            }
        }
        return p;
    }

    unsafe { f(x, n).offset_from(x.as_ptr()) as usize }
}

/// Find the character at position `i`.
pub(crate) fn at(x: &str, i: CharIndex) -> Option<char> {
    if i.0 as usize >= x.len() {
        // Important that skip_at_most_1byte gets called with all valid character.
        // If the index is outside the length even under the best assumptions,
        // can immediately return None.
        return None;
    }
    let n = skip_at_most_1byte(x, i.0 as usize);
    let s = unsafe { x.get_unchecked(n..) };
    s.chars().nth(i.0 as usize - n)
}

/// Find the length of the string in characters.
/// If the length matches the length in bytes, the string must be 7bit ASCII.
pub(crate) fn len(x: &str) -> CharIndex {
    let n = skip_at_most_1byte(x, x.len());
    if n == x.len() {
        CharIndex(n) // All 1 byte
    } else {
        CharIndex(unsafe { x.get_unchecked(n..) }.chars().count() + n)
    }
}

/// Find the number of times a `needle` byte occurs within a string.
/// If the needle represents a complete character, this will be equivalent to doing
/// search for that character in the string.
pub fn count_matches_byte(x: &str, needle: u8) -> usize {
    x.as_bytes().iter().filter(|x| **x == needle).count()
}

/// Find the number of times a `needle` occurs within a string, non-overlapping.
pub fn count_matches(x: &str, needle: &str) -> usize {
    if needle.len() == 1 {
        // If we are searching for a 1-byte string, we can provide a much faster path.
        // Since it is one byte, given how UTF8 works, all the resultant slices must be UTF8 too.
        count_matches_byte(x, needle.as_bytes()[0])
    } else {
        x.matches(needle).count()
    }
}

/// Result of applying `start` and `end` to a string.
#[derive(PartialEq, Debug)]
pub(crate) struct StrIndices<'a> {
    /// Computed start char index.
    pub(crate) start: CharIndex,
    /// Substring after applying the `start` and `end` arguments.
    pub(crate) haystack: &'a str,
}

/// Split the string at given char offset. `None` if offset is out of bounds.
pub(crate) fn split_at(x: &str, i: CharIndex) -> Option<(&str, &str)> {
    if i.0 == 0 {
        return Some(("", x));
    }
    if i.0 > x.len() {
        return None;
    }
    let n = skip_at_most_1byte(x, i.0);
    let s = unsafe { x.get_unchecked(n..) };
    let mut c = s.chars();
    for _ in 0..i.0 - n {
        c.next()?;
    }
    Some(x.split_at(x.len() - c.as_str().len()))
}

/// Perform the Starlark operation `x[:i]` (`i` is an unsigned integer here).
fn split_at_end(x: &str, i: CharIndex) -> &str {
    match split_at(x, i) {
        Some((before, _)) => before,
        None => x,
    }
}

fn convert_str_indices_slow(s: &str, start: NoneOr<i32>, end: NoneOr<i32>) -> Option<StrIndices> {
    // Slow version when we need to compute full string length
    // because at least one of the indices is negative.
    debug_assert!(
        matches!(start, NoneOr::Other(start) if start < 0)
            || matches!(end, NoneOr::Other(end) if end < 0)
    );
    // If both indices are negative, we should have ruled `start > end` case before.
    debug_assert!(
        matches!((start, end), (NoneOr::Other(start), NoneOr::Other(end))
                if start >= 0 || end >= 0 || (start <= end))
            || matches!(start, NoneOr::None)
            || matches!(end, NoneOr::None)
    );
    let len = len(s);
    let (start, end) = convert_indices(len.0 as i32, start, end);
    if start > end {
        return None;
    }
    let (start, end) = (CharIndex(start), CharIndex(end));
    debug_assert!(end <= len);
    let s = if len.0 == s.len() {
        // ASCII fast path: if char len is equal to byte len,
        // we know the string is ASCII.
        unsafe { s.get_unchecked(start.0..end.0) }
    } else {
        let (_, s) = split_at(s, start).unwrap();
        let (s, _) = split_at(s, end - start).unwrap();
        s
    };
    Some(StrIndices { start, haystack: s })
}

/// Convert common `start` and `end` arguments of `str` functions like `str.find`.
#[inline(always)]
pub(crate) fn convert_str_indices(
    s: &str,
    start: NoneOr<i32>,
    end: NoneOr<i32>,
) -> Option<StrIndices> {
    match (start, end) {
        // Following cases but last optimize index computation
        // by avoiding computing the length of the string.
        (NoneOr::None, NoneOr::None) => Some(StrIndices {
            start: CharIndex(0),
            haystack: s,
        }),
        (NoneOr::Other(start), NoneOr::None) if start >= 0 => {
            let (_, s) = split_at(s, CharIndex(start as usize))?;
            Some(StrIndices {
                start: CharIndex(start as usize),
                haystack: s,
            })
        }
        (NoneOr::None, NoneOr::Other(end)) if end >= 0 => {
            let s = split_at_end(s, CharIndex(end as usize));
            Some(StrIndices {
                start: CharIndex(0),
                haystack: s,
            })
        }
        (NoneOr::Other(start), NoneOr::Other(end)) if start >= 0 && end >= start => {
            let (_, s) = split_at(s, CharIndex(start as usize))?;
            let s = split_at_end(s, CharIndex((end - start) as usize));
            Some(StrIndices {
                start: CharIndex(start as usize),
                haystack: s,
            })
        }
        (NoneOr::Other(start), NoneOr::Other(end))
            if ((start >= 0) == (end >= 0)) && start > end =>
        {
            None
        }
        (start, end) => convert_str_indices_slow(s, start, end),
    }
}

pub(crate) fn contains(haystack: &str, needle: &str) -> bool {
    if needle.is_empty() {
        true
    } else if needle.len() == 1 {
        memchr::memchr(needle.as_bytes()[0], haystack.as_bytes()).is_some()
    } else if haystack.len() < needle.len() {
        false
    } else {
        assert!(haystack.len() >= needle.len());
        // `str::contains` is very slow for short strings.
        // So use basic quadratic algorithm instead.
        let needle_0 = needle.as_bytes()[0];
        for start in 0..=haystack.len() - needle.len() {
            if haystack.as_bytes()[start] != needle_0 {
                continue;
            }
            if haystack.as_bytes()[start..].starts_with(needle.as_bytes()) {
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use crate::values::{
        string::fast_string::convert_str_indices,
        types::{
            none::NoneOr,
            string::{fast_string::StrIndices, CharIndex},
        },
    };

    #[test]
    fn test_convert_str_indices() {
        assert_eq!(
            Some(StrIndices {
                start: CharIndex(0),
                haystack: "abc",
            }),
            convert_str_indices("abc", NoneOr::None, NoneOr::None)
        );
        assert_eq!(
            None,
            convert_str_indices("abc", NoneOr::Other(2), NoneOr::Other(1))
        );

        assert_eq!(
            Some(StrIndices {
                start: CharIndex(0),
                haystack: "abc",
            }),
            convert_str_indices("abc", NoneOr::Other(-10), NoneOr::Other(10))
        );
        assert_eq!(
            Some(StrIndices {
                start: CharIndex(1),
                haystack: "",
            }),
            convert_str_indices("abc", NoneOr::Other(1), NoneOr::Other(1))
        );
        assert_eq!(
            Some(StrIndices {
                start: CharIndex(0),
                haystack: "ab",
            }),
            convert_str_indices("abc", NoneOr::Other(-10), NoneOr::Other(2))
        );
        assert_eq!(
            Some(StrIndices {
                start: CharIndex(0),
                haystack: "ab",
            }),
            convert_str_indices("abc", NoneOr::Other(-10), NoneOr::Other(-1))
        );

        assert_eq!(
            Some(StrIndices {
                start: CharIndex(0),
                haystack: "s",
            }),
            convert_str_indices("short", NoneOr::Other(0), NoneOr::Other(-4))
        );
        assert_eq!(
            Some(StrIndices {
                start: CharIndex(0),
                haystack: "fish",
            }),
            convert_str_indices("fish", NoneOr::None, NoneOr::Other(10))
        );
    }

    #[test]
    fn test_convert_str_indices_non_ascii() {
        assert_eq!(
            Some(StrIndices {
                start: CharIndex(6),
                haystack: "под",
            }),
            convert_str_indices("Город под подошвой", NoneOr::Other(6), NoneOr::Other(9))
        )
    }

    #[test]
    fn test_convert_str_indices_trigger_debug_assertions() {
        fn none_ors() -> impl Iterator<Item = NoneOr<i32>> {
            iter::once(NoneOr::None).chain((-30..30).map(NoneOr::Other))
        }

        for s in &["", "a", "abcde", "Телемак"] {
            for start in none_ors() {
                for end in none_ors() {
                    let _ = convert_str_indices(s, start, end);
                }
            }
        }
    }
}
