/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::provider::label::ConfiguredProvidersLabel;
use small_map::set::SmallSet;

use crate::attrs::attr_type::query::ResolvedQueryLiterals;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;

#[derive(Default, Debug)]
pub struct ConfiguredAttrInfo {
    // Including transitioned deps.
    pub deps: SmallSet<ConfiguredProvidersLabel>,
    pub execution_deps: SmallSet<ConfiguredProvidersLabel>,
    pub has_query: bool,
}

impl ConfiguredAttrInfo {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<'a> ConfiguredAttrTraversal<'a> for ConfiguredAttrInfo {
    fn dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        self.deps.insert(dep.clone());
        Ok(())
    }

    fn query_macro(
        &mut self,
        _query: &'a str,
        _resolved_literals: &'a ResolvedQueryLiterals<ConfiguredAttr>,
    ) -> anyhow::Result<()> {
        self.has_query = true;
        Ok(())
    }

    fn exec_dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        self.execution_deps.insert(dep.clone());
        Ok(())
    }
}
