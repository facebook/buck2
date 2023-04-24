/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;

use allocative::Allocative;

/// A 'CellAlias' is a user-provided string name that maps to a 'CellName'.
/// The mapping of 'CellAlias' to 'CellName' is specific to the current cell so
/// that the same 'CellAlias' may map to different 'CellName's depending on what
/// the current 'CellInstance' is that references the 'CellAlias'.
#[derive(
    Clone,
    Debug,
    derive_more::Display,
    Hash,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Allocative
)]
pub struct CellAlias(String);

impl CellAlias {
    pub fn new(alias: String) -> CellAlias {
        CellAlias(alias)
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for CellAlias {
    fn borrow(&self) -> &str {
        &self.0
    }
}
