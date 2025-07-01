/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::package::PackageLabel;
use buck2_query::query::environment::AttrFmtOptions;

/// Attribute formatting context (for `Display` or `Serialize`).
pub struct AttrFmtContext {
    /// `None` when there's no package, for example when formatting for:
    /// * default value for attribute definition
    /// * tests
    /// * error messages
    pub package: Option<PackageLabel>,
    pub options: AttrFmtOptions,
}

impl AttrFmtContext {
    pub const NO_CONTEXT: AttrFmtContext = AttrFmtContext {
        package: None,
        options: AttrFmtOptions {
            exclude_quotes: false,
        },
    };
}
