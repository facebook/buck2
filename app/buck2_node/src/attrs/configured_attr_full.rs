/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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

impl<'a> ConfiguredAttrFull<'a> {
    pub fn traverse<'v>(
        &'v self,
        pkg: PackageLabel,
        traversal: &mut dyn ConfiguredAttrTraversal,
    ) -> buck2_error::Result<()> {
        self.value
            .traverse(pkg, traversal)
            .with_buck_error_context(|| format!("traversing attribute `{}`", self.name))
    }
}
