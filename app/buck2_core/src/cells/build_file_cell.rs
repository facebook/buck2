/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use ref_cast::RefCast;

use crate::cells::CellName;

/// Used to hold the cell name for the top-level build file being interpreted.
/// There's a subtlety where that doesn't necessarily match the cell of the file
/// being interpreted. This gets its own type because its easy to get wrong.
#[derive(
    Clone,
    Hash,
    Eq,
    PartialEq,
    Debug,
    derive_more::Display,
    RefCast,
    Allocative
)]
#[display(fmt = "{}", .0)]
#[repr(C)]
pub struct BuildFileCell(CellName);

impl BuildFileCell {
    pub fn new(name: CellName) -> Self {
        Self(name)
    }

    pub fn name(&self) -> &CellName {
        &self.0
    }
}
