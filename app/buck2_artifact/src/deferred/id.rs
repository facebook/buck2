/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use dupe::Dupe;

/// An id to look up the deferred work
#[derive(Clone, Copy, Debug, Dupe, derive_more::Display, Allocative)]
// comment because linters and fmt don't agree
#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
#[display("{}", id)]
pub struct DeferredId {
    pub id: u32,
    pub trivial: bool,
}

impl DeferredId {
    /// Gets the underlying ID for this DeferredId. This should be used for logging only.
    #[inline]
    pub fn as_usize(self) -> usize {
        self.id as _
    }

    #[inline]
    pub fn is_trivial(self) -> bool {
        self.trivial
    }

    pub fn testing_new(id: u32) -> DeferredId {
        DeferredId { id, trivial: false }
    }
}
