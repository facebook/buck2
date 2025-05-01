/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Deref;
use std::str;

use allocative::Allocative;
use dupe::Dupe;
use serde::Serialize;
use static_assertions::assert_eq_size;

use crate::arc_str::base::ArcStrBase;
use crate::arc_str::base::ArcStrBaseInner;
use crate::arc_str::base::ArcStrBaseInnerConst;
use crate::arc_str::base::ArcStrLenStrategy;

struct ThinArcStrProperties;

unsafe impl ArcStrLenStrategy for ThinArcStrProperties {
    type AllocatedPayload = u32;
    type ValuePayload = ();
    const EMPTY: (Self::AllocatedPayload, Self::ValuePayload) = (0, ());

    #[inline]
    fn unpack_len(d: (Self::AllocatedPayload, Self::ValuePayload)) -> u32 {
        d.0
    }

    #[inline]
    fn pack_len(len: u32) -> (Self::AllocatedPayload, Self::ValuePayload) {
        (len, ())
    }

    #[inline]
    fn inner_empty() -> &'static ArcStrBaseInnerConst<Self> {
        &INNER_EMPTY
    }
}

/// Wrapper for `Arc<str>`.
#[derive(
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    Allocative,
    Clone,
    Dupe,
    Default,
    strong_hash::StrongHash
)]
pub struct ThinArcStr {
    base: ArcStrBase<ThinArcStrProperties>,
}

assert_eq_size!(ThinArcStr, usize);
assert_eq_size!(Option<ThinArcStr>, usize);

static INNER_EMPTY: ArcStrBaseInnerConst<ThinArcStrProperties> = ArcStrBaseInner::EMPTY;

impl ThinArcStr {
    #[inline]
    pub fn as_str(&self) -> &str {
        self.base.as_str()
    }
}

impl Deref for ThinArcStr {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<str> for ThinArcStr {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Borrow<str> for ThinArcStr {
    #[inline]
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Display for ThinArcStr {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl Debug for ThinArcStr {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

impl<'a> From<&'a str> for ThinArcStr {
    #[inline]
    fn from(s: &'a str) -> ThinArcStr {
        ThinArcStr {
            base: ArcStrBase::from(s),
        }
    }
}

impl From<String> for ThinArcStr {
    #[inline]
    fn from(s: String) -> ThinArcStr {
        ThinArcStr::from(s.as_str())
    }
}

impl Serialize for ThinArcStr {
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

    use crate::arc_str::thin::ThinArcStr;

    #[test]
    fn test_serialize() {
        assert_eq!(
            "\"hello\"",
            serde_json::to_string(&ThinArcStr::from("hello")).unwrap()
        );
        assert_eq!(
            "\"\"",
            serde_json::to_string(&ThinArcStr::from("")).unwrap()
        );
    }

    #[test]
    fn test_hash() {
        fn hash<H: Hash + ?Sized>(h: &H) -> u64 {
            let mut hasher = DefaultHasher::new();
            h.hash(&mut hasher);
            hasher.finish()
        }

        assert_eq!(hash(""), hash(&ThinArcStr::from("")));
        assert_eq!(hash("hello"), hash(&ThinArcStr::from("hello")));
    }

    #[test]
    fn test_clone_drop_ref_count() {
        fn ref_count(s: &ThinArcStr) -> u32 {
            assert!(!s.is_empty());
            s.base.inner().refcount.load(atomic::Ordering::Relaxed)
        }

        let s = ThinArcStr::from("hello");
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
                .map(|i| ThinArcStr::from(i.to_string()))
                .collect::<Vec<_>>();
            for j in 0..1000 {
                let _ignore = ThinArcStr::from(j.to_string());
            }
            for (k, s) in strings.iter().enumerate() {
                assert_eq!(k.to_string(), s.as_str());
            }
        }
    }
}
