/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use core::slice;
use std::alloc;
use std::alloc::Layout;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::mem;
use std::ops::Deref;
use std::ptr;
use std::ptr::NonNull;
use std::str;
use std::sync::atomic;
use std::sync::atomic::AtomicU32;
use std::sync::Arc;

use allocative::Allocative;
use buck2_util::rtabort;
use dupe::Dupe;
use serde::Serialize;
use static_assertions::assert_eq_size;

#[repr(C)]
struct ArcStrInner {
    counter: AtomicU32,
    // data: [],
}

impl ArcStrInner {
    const OFFSET_OF_DATA: usize = mem::size_of::<ArcStrInner>();

    fn layout_for_len(len: usize) -> Layout {
        let size = Self::OFFSET_OF_DATA.checked_add(len).unwrap();
        let align = mem::align_of::<ArcStrInner>();
        Layout::from_size_align(size, align).unwrap()
    }

    unsafe fn layout_for_len_unchecked(len: usize) -> Layout {
        let size = Self::OFFSET_OF_DATA + len;
        let align = mem::align_of::<ArcStrInner>();
        unsafe { Layout::from_size_align_unchecked(size, align) }
    }
}

/// Wrapper for `Arc<str>`.
pub struct ArcStr {
    /// Points to the end of `ArcStrInner` struct.
    /// Dangling pointer when empty.
    data: NonNull<u8>,
    len: usize,
}

unsafe impl Send for ArcStr {}
unsafe impl Sync for ArcStr {}

impl Default for ArcStr {
    fn default() -> ArcStr {
        ArcStr::EMPTY
    }
}

assert_eq_size!(ArcStr, Arc<str>);
assert_eq_size!(Option<ArcStr>, Arc<str>);

impl ArcStr {
    /// Empty string.
    pub const EMPTY: ArcStr = ArcStr {
        data: NonNull::dangling(),
        len: 0,
    };

    #[inline]
    pub fn as_str(&self) -> &str {
        unsafe {
            let bytes = slice::from_raw_parts(self.data.as_ptr(), self.len);
            str::from_utf8_unchecked(bytes)
        }
    }
}

impl Deref for ArcStr {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<str> for ArcStr {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Hash for ArcStr {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl PartialEq for ArcStr {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.data.as_ptr(), other.data.as_ptr()) || self.as_str() == other.as_str()
    }
}

impl Eq for ArcStr {}

impl PartialOrd for ArcStr {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl Ord for ArcStr {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl Borrow<str> for ArcStr {
    #[inline]
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Display for ArcStr {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl Debug for ArcStr {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

impl<'a> From<&'a str> for ArcStr {
    fn from(s: &'a str) -> ArcStr {
        if s.is_empty() {
            ArcStr::EMPTY
        } else {
            let layout = ArcStrInner::layout_for_len(s.len());
            unsafe {
                let alloc = alloc::alloc(layout);
                ptr::write(
                    alloc.cast::<ArcStrInner>(),
                    ArcStrInner {
                        counter: AtomicU32::new(1),
                    },
                );
                let data = alloc.add(ArcStrInner::OFFSET_OF_DATA);
                ptr::copy_nonoverlapping(s.as_ptr(), data, s.len());
                ArcStr {
                    data: NonNull::new(data).unwrap(),
                    len: s.len(),
                }
            }
        }
    }
}

impl Clone for ArcStr {
    fn clone(&self) -> Self {
        if self.len == 0 {
            ArcStr::EMPTY
        } else {
            unsafe {
                let arc_str_inner = &*self
                    .data
                    .as_ptr()
                    .sub(ArcStrInner::OFFSET_OF_DATA)
                    .cast::<ArcStrInner>();
                // This is what `Arc::clone` does.
                let old_count = arc_str_inner
                    .counter
                    .fetch_add(1, atomic::Ordering::Relaxed);
                const MAX_REFCOUNT: u32 = i32::MAX as u32;
                if old_count > MAX_REFCOUNT {
                    arc_str_inner
                        .counter
                        .fetch_sub(1, atomic::Ordering::Relaxed);
                    rtabort!("refcount overflow");
                }
                ArcStr {
                    data: self.data,
                    len: self.len,
                }
            }
        }
    }
}

impl Dupe for ArcStr {}

impl Drop for ArcStr {
    fn drop(&mut self) {
        if self.len != 0 {
            unsafe {
                let arc_str_inner = self
                    .data
                    .as_ptr()
                    .sub(ArcStrInner::OFFSET_OF_DATA)
                    .cast::<ArcStrInner>();
                // Again, copy-paste from `Arc::drop`.
                if (*arc_str_inner)
                    .counter
                    .fetch_sub(1, atomic::Ordering::Release)
                    != 1
                {
                    return;
                }
                atomic::fence(atomic::Ordering::Acquire);
                let layout = ArcStrInner::layout_for_len_unchecked(self.len);
                alloc::dealloc(arc_str_inner.cast::<u8>(), layout);
            }
        }
    }
}

impl From<String> for ArcStr {
    fn from(s: String) -> ArcStr {
        ArcStr::from(s.as_str())
    }
}

impl Allocative for ArcStr {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        if self.len != 0 {
            if let Some(mut visitor) = visitor.enter_shared(
                allocative::Key::new("data"),
                mem::size_of::<*const u8>(),
                self.data.as_ptr() as *const (),
            ) {
                visitor.visit_simple(
                    allocative::Key::new("ArcStrInner"),
                    mem::size_of::<ArcStrInner>(),
                );
                visitor.visit_simple(allocative::Key::new("data"), self.len);
                visitor.exit();
            }
        }
        visitor.exit();
    }
}

impl Serialize for ArcStr {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.as_str().serialize(serializer)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hash;
    use std::hash::Hasher;
    use std::sync::atomic;

    use dupe::Dupe;

    use crate::arc_str::ArcStr;
    use crate::arc_str::ArcStrInner;

    #[test]
    fn test_serialize() {
        assert_eq!(
            "\"hello\"",
            serde_json::to_string(&ArcStr::from("hello")).unwrap()
        );
        assert_eq!("\"\"", serde_json::to_string(&ArcStr::from("")).unwrap());
    }

    #[test]
    fn test_hash() {
        fn hash<H: Hash + ?Sized>(h: &H) -> u64 {
            let mut hasher = DefaultHasher::new();
            h.hash(&mut hasher);
            hasher.finish()
        }

        assert_eq!(hash(""), hash(&ArcStr::from("")));
        assert_eq!(hash("hello"), hash(&ArcStr::from("hello")));
    }

    #[test]
    fn test_clone_drop_ref_count() {
        fn ref_count(s: &ArcStr) -> u32 {
            assert!(!s.is_empty());
            unsafe {
                (*(s.data.as_ptr().sub(ArcStrInner::OFFSET_OF_DATA) as *const ArcStrInner))
                    .counter
                    .load(atomic::Ordering::Relaxed)
            }
        }

        let s = ArcStr::from("hello");
        assert_eq!(1, ref_count(&s));
        let s2 = s.dupe();
        assert_eq!(2, ref_count(&s));
        drop(s);
        assert_eq!(1, ref_count(&s2));
    }

    #[test]
    fn test_little_stress_test() {
        for i in 0..100 {
            let strings = (0..i)
                .map(|i| ArcStr::from(i.to_string()))
                .collect::<Vec<_>>();
            for j in 0..1000 {
                let _ignore = ArcStr::from(j.to_string());
            }
            for (k, s) in strings.iter().enumerate() {
                assert_eq!(k.to_string(), s.as_str());
            }
        }
    }
}
