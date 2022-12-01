/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::buck_path::BuckPathRef;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::TargetLabel;

use crate::attrs::attr_type::query::ResolvedQueryLiterals;
use crate::attrs::configured_attr::ConfiguredAttr;

pub trait ConfiguredAttrTraversal<'a> {
    fn dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()>;

    fn exec_dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        // By default, just treat it as a dep. Most things don't care about the distinction.
        self.dep(dep)
    }

    fn toolchain_dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        // By default, just treat it as a dep. Most things don't care about the distinction.
        self.dep(dep)
    }

    fn configuration_dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
        Ok(())
    }

    fn query_macro(
        &mut self,
        _query: &'a str,
        _resolved_literals: &'a ResolvedQueryLiterals<ConfiguredAttr>,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    fn input(&mut self, _path: BuckPathRef) -> anyhow::Result<()> {
        Ok(())
    }

    fn label(&mut self, _label: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        Ok(())
    }
}
