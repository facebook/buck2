/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::fmt::Display;

/// The starlark interpreter expects imports
/// to be identified by a String and requires using this id in some cases.
/// The id will contain both the package and filename. For a cross-cell load,
/// it will include an `@cell_name` suffix to indicate the top-level cell
/// being loaded into.
#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct ModuleID(pub String);

impl Display for ModuleID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Borrow<str> for ModuleID {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl ModuleID {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}
