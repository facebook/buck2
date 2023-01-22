/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use derive_more::Display;

/// A 'CellName' is a canonicalized, human-readable name that corresponds to a
/// 'CellInstance'. There should be a one to one mapping between a 'CellName'
/// and a 'CellInstance'.
///
/// The cell within a fully qualified target like `foo//some:target` is `foo`.
/// The cell name is also restricted to alphabet characters (i.e. shouldn't
/// contain any special characters like `/`), so `foo/bar//some:target` has an
/// invalid cell name of `foo/bar`.
// TODO consider if we need to intern the string
#[derive(
    Clone, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative
)]
pub struct CellName(String);

impl CellName {
    pub fn unchecked_new(name: String) -> CellName {
        CellName(name)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}
