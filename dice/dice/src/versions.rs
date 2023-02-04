/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! This library contains utilities for tracking a global version number. The
//! global version number is used for tagging computed values so that we can
//! track when a value needs to be updated because its version number is out of
//! date.

use std::fmt::Debug;
use std::ops::Sub;

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;

/// The incrementing Version number associated with all the cache entries
#[derive(Copy, Eq, Debug, Display, Dupe)]
// split this due to formatters not agreeing
#[derive(PartialEq, Hash, Clone, Ord, PartialOrd, Allocative)]
#[display(fmt = "v{}", "_0")]
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
