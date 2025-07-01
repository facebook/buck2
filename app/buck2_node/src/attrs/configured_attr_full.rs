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
use buck2_error::BuckErrorContext;

use crate::attrs::attr::Attribute;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;

/// Full configured attribute: name, type, value.
#[derive(Debug)]
pub struct ConfiguredAttrFull<'a> {
    pub name: &'a str,
    pub attr: &'a Attribute,
    pub value: ConfiguredAttr,
}

impl ConfiguredAttrFull<'_> {
    pub fn traverse(
        &self,
        pkg: PackageLabel,
        traversal: &mut dyn ConfiguredAttrTraversal,
    ) -> buck2_error::Result<()> {
        self.value
            .traverse(pkg, traversal)
            .with_buck_error_context(|| format!("traversing attribute `{}`", self.name))
    }
}
