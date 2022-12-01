/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

use allocative::Allocative;
use gazebo::dupe::Dupe;
use serde::Serialize;
use static_assertions::assert_eq_size;

/// Wrapper for `Arc<str>`.
#[derive(Clone, Dupe, Allocative)]
#[derive(Default)]
pub struct ArcStr(
    /// `None` is used to represent the empty string.
    Option<Arc<str>>,
);

assert_eq_size!(ArcStr, Arc<str>);

impl ArcStr {
    /// Empty string.
    pub const EMPTY: ArcStr = ArcStr(None);

    pub fn as_str(&self) -> &str {
        match &self.0 {
            Some(s) => s,
            None => "",
        }
    }
}

impl Deref for ArcStr {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<str> for ArcStr {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Hash for ArcStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl PartialEq for ArcStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for ArcStr {}

impl PartialOrd for ArcStr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl Ord for ArcStr {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl Borrow<str> for ArcStr {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Display for ArcStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl Debug for ArcStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

impl<'a> From<&'a str> for ArcStr {
    fn from(s: &'a str) -> ArcStr {
        if s.is_empty() {
            ArcStr::default()
        } else {
            ArcStr(Some(Arc::from(s)))
        }
    }
}

impl From<String> for ArcStr {
    fn from(s: String) -> ArcStr {
        ArcStr::from(s.as_str())
    }
}

impl From<Arc<str>> for ArcStr {
    fn from(s: Arc<str>) -> ArcStr {
        if s.is_empty() {
            ArcStr::default()
        } else {
            ArcStr(Some(s))
        }
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

    use crate::arc_str::ArcStr;

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
}
