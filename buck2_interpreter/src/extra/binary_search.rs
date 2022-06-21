// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use std::cmp::Ordering::*;
use std::cmp::Ordering::{self};

// code adapted from Rust's std::slice::binary_search_by itself under MIT License
// https://github.com/rust-lang/rust/blob/0d93d3f4a42fd45b2a0da658d39555316a1b6793/src/libcore/slice/mod.rs
// If the value is found then [`Result::Ok`] is returned, containing the
// index of the matching element. If there are multiple matches, then any
// one of the matches could be returned. If the value is not found then
// [`Result::Err`] is returned, containing the index where a matching
// element could be inserted while maintaining sorted order.
pub(crate) fn binary_search_by<F>(len: usize, mut f: F) -> Result<usize, usize>
where
    F: FnMut(usize) -> Ordering,
{
    let mut size = len;
    if size == 0 {
        return Err(0);
    }
    let mut base = 0usize;
    while size > 1 {
        let half = size / 2;
        let mid = base + half;
        // mid is always in [0, size), that means mid is >= 0 and < size.
        // mid >= 0: by definition
        // mid < size: mid = size / 2 + size / 4 + size / 8 ...
        let cmp = f(mid);
        base = if cmp == Greater { base } else { mid };
        size -= half;
    }
    // base is always in [0, size) because base <= mid.
    let cmp = f(base);
    if cmp == Equal {
        Ok(base)
    } else {
        Err(base + (cmp == Less) as usize)
    }
}
