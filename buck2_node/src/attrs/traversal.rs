/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::buck_path::BuckPathRef;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::TargetLabel;

pub trait CoercedAttrTraversal<'a> {
    fn dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()>;
    fn exec_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()>;
    fn toolchain_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()>;
    fn transition_dep(
        &mut self,
        dep: &'a TargetLabel,
        tr: &'a Arc<TransitionId>,
    ) -> anyhow::Result<()>;
    fn split_transition_dep(
        &mut self,
        dep: &'a TargetLabel,
        tr: &'a Arc<TransitionId>,
    ) -> anyhow::Result<()>;
    fn configuration_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()>;
    fn platform_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()>;
    fn input(&mut self, input: BuckPathRef) -> anyhow::Result<()>;
    fn label(&mut self, _label: &'a ProvidersLabel) -> anyhow::Result<()> {
        Ok(())
    }
}
