/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use crate::fs::paths::file_name::FileName;
use crate::fs::paths::file_name::FileNameBuf;

/// Accumulate path components in reverse order. This is used to show the path where an issue
/// occurred in Directory operations.
pub struct PathAccumulator {
    inner: Vec<FileNameBuf>,
}

impl PathAccumulator {
    pub fn new(p: &FileName) -> Self {
        PathAccumulator {
            inner: vec![p.to_owned()],
        }
    }

    pub fn with(mut self, p: &FileName) -> Self {
        self.inner.push(p.to_owned());
        self
    }
}

impl fmt::Debug for PathAccumulator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Dedicated Debug since we reverse the order so a derived debug would be less than useful.
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for PathAccumulator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for e in self.inner.iter().rev() {
            if !first {
                write!(f, "/")?;
            }
            first = false;
            write!(f, "{}", e)?;
        }
        Ok(())
    }
}

impl<'a> IntoIterator for &'a PathAccumulator {
    type Item = &'a FileName;
    type IntoIter = impl Iterator<Item = Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter().rev().map(|e| e.as_ref())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_accumulator() {
        let p = PathAccumulator::new(FileName::unchecked_new("foo"));
        assert_eq!(p.to_string(), "foo");

        let p = p.with(FileName::unchecked_new("bar"));
        assert_eq!(p.to_string(), "bar/foo");

        let mut iter = (&p).into_iter();
        assert_eq!(iter.next().unwrap().to_string(), "bar");
        assert_eq!(iter.next().unwrap().to_string(), "foo");
        assert_eq!(iter.next(), None);
    }
}
