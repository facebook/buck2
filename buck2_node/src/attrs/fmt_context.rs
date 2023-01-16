/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::package::PackageLabel;

/// Attribute formatting context (for `Display` or `Serialize`).
pub struct AttrFmtContext {
    /// `None` when there's no package, for example when formatting for:
    /// * default value for attribute definition
    /// * tests
    /// * error messages
    pub package: Option<PackageLabel>,
}

impl AttrFmtContext {
    pub const NO_CONTEXT: AttrFmtContext = AttrFmtContext { package: None };
}
