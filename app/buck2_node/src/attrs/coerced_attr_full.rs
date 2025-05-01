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
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr_full::ConfiguredAttrFull;
use crate::attrs::traversal::CoercedAttrTraversal;

/// Full coerced attribute: name, type, value.
pub struct CoercedAttrFull<'a> {
    pub name: &'a str,
    pub attr: &'a Attribute,
    pub value: &'a CoercedAttr,
}

impl<'a> CoercedAttrFull<'a> {
    pub fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<ConfiguredAttrFull<'a>> {
        Ok(ConfiguredAttrFull {
            name: self.name,
            attr: self.attr,
            value: self
                .value
                .configure(self.attr.coercer(), ctx)
                .with_buck_error_context(|| format!("configuring attr `{}`", self.name))?,
        })
    }

    pub fn traverse(
        &self,
        pkg: PackageLabel,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        self.value
            .traverse(self.attr.coercer(), pkg, traversal)
            .with_buck_error_context(|| format!("traversing attribute `{}`", self.name))
    }
}
