// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::{config::MAX_INLINE, marker_byte::Marker};
use core::{
    slice::{from_raw_parts, from_raw_parts_mut},
    str::{from_utf8_unchecked, from_utf8_unchecked_mut},
};

#[repr(C)]
pub(crate) struct InlineString {
    pub(crate) marker: Marker,
    pub(crate) data: [u8; MAX_INLINE],
}

impl Clone for InlineString {
    fn clone(&self) -> Self {
        unreachable!("InlineString should be copy!")
    }
}

impl Copy for InlineString {}

impl InlineString {
    pub(crate) const fn new() -> Self {
        Self {
            marker: Marker::empty(),
            data: [0; MAX_INLINE],
        }
    }

    pub(crate) fn set_size(&mut self, size: usize) {
        self.marker.set_data(size as u8);
    }

    pub(crate) fn len(&self) -> usize {
        let len = self.marker.data() as usize;
        // Panic immediately if inline length is too high, which suggests
        // assumptions made about `String`'s memory layout are invalid.
        assert!(len <= MAX_INLINE);
        len
    }

    pub(crate) fn as_slice(&self) -> &[u8] {
        self.data.as_ref()
    }

    pub(crate) fn as_mut_slice(&mut self) -> &mut [u8] {
        self.data.as_mut()
    }

    pub(crate) fn as_str(&self) -> &str {
        unsafe {
            let data = from_raw_parts(self.data.as_ref().as_ptr(), self.len());
            from_utf8_unchecked(data)
        }
    }

    pub(crate) fn as_mut_str(&mut self) -> &mut str {
        unsafe {
            let data = from_raw_parts_mut(self.data.as_mut().as_mut_ptr(), self.len());
            from_utf8_unchecked_mut(data)
        }
    }

    pub(crate) fn insert_bytes(&mut self, index: usize, bytes: &[u8]) {
        assert!(self.as_str().is_char_boundary(index));
        if bytes.is_empty() {
            return;
        }
        let len = self.len();
        unsafe {
            let ptr = self.data.as_mut().as_mut_ptr();
            if index != len {
                ptr.add(index + bytes.len())
                    .copy_from(&self.data.as_ref()[index], len - index);
            }
            ptr.add(index)
                .copy_from_nonoverlapping(bytes.as_ptr(), bytes.len());
        }
        self.set_size(len + bytes.len());
    }

    pub(crate) fn remove_bytes(&mut self, start: usize, end: usize) {
        let len = self.len();
        assert!(start <= end);
        assert!(end <= len);
        assert!(self.as_str().is_char_boundary(start));
        assert!(self.as_str().is_char_boundary(end));
        if start == end {
            return;
        }
        if end < len {
            unsafe {
                let ptr = self.data.as_mut().as_mut_ptr();
                ptr.add(start).copy_from(ptr.add(end), len - end);
            }
        }
        self.set_size(len - (end - start));
    }
}

impl From<&'_ [u8]> for InlineString {
    fn from(bytes: &[u8]) -> Self {
        let len = bytes.len();
        debug_assert!(len <= MAX_INLINE);
        let mut out = Self::new();
        out.marker = Marker::new_inline(len as u8);
        out.data.as_mut()[..len].copy_from_slice(bytes);
        out
    }
}
