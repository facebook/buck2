/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::pin::Pin;

use async_trait::async_trait;
use buck2_core::cells::name::CellName;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::target::label::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::compatibility::MaybeCompatible;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::functions::helpers::CapturedExpr;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

#[async_trait]
pub trait BxlCqueryFunctions<'c>: Send + 'c {
    async fn allpaths(
        &self,
        from: &TargetSet<ConfiguredTargetNode>,
        to: &TargetSet<ConfiguredTargetNode>,
    ) -> anyhow::Result<TargetSet<ConfiguredTargetNode>>;
    async fn somepath(
        &self,
        from: &TargetSet<ConfiguredTargetNode>,
        to: &TargetSet<ConfiguredTargetNode>,
    ) -> anyhow::Result<TargetSet<ConfiguredTargetNode>>;
    async fn owner(&self, file_set: &FileSet) -> anyhow::Result<TargetSet<ConfiguredTargetNode>>;
    async fn deps(
        &self,
        targets: &TargetSet<ConfiguredTargetNode>,
        deps: Option<i32>,
        captured_expr: Option<&CapturedExpr>,
    ) -> anyhow::Result<TargetSet<ConfiguredTargetNode>>;
    async fn rdeps(
        &self,
        universe: &TargetSet<ConfiguredTargetNode>,
        targets: &TargetSet<ConfiguredTargetNode>,
        depth: Option<i32>,
    ) -> anyhow::Result<TargetSet<ConfiguredTargetNode>>;
    async fn testsof(
        &self,
        targets: &TargetSet<ConfiguredTargetNode>,
    ) -> anyhow::Result<TargetSet<ConfiguredTargetNode>>;
    async fn testsof_with_default_target_platform(
        &self,
        targets: &TargetSet<ConfiguredTargetNode>,
    ) -> anyhow::Result<Vec<MaybeCompatible<ConfiguredTargetNode>>>;
}

#[async_trait]
pub trait BxlUqueryFunctions<'c>: Send + 'c {
    async fn allpaths(
        &self,
        from: &TargetSet<TargetNode>,
        to: &TargetSet<TargetNode>,
    ) -> anyhow::Result<TargetSet<TargetNode>>;
    async fn somepath(
        &self,
        from: &TargetSet<TargetNode>,
        to: &TargetSet<TargetNode>,
    ) -> anyhow::Result<TargetSet<TargetNode>>;
    async fn deps(
        &self,
        targets: &TargetSet<TargetNode>,
        deps: Option<i32>,
        captured_expr: Option<&CapturedExpr>,
    ) -> anyhow::Result<TargetSet<TargetNode>>;
    async fn rdeps(
        &self,
        universe: &TargetSet<TargetNode>,
        targets: &TargetSet<TargetNode>,
        depth: Option<i32>,
    ) -> anyhow::Result<TargetSet<TargetNode>>;
    async fn testsof(
        &self,
        targets: &TargetSet<TargetNode>,
    ) -> anyhow::Result<TargetSet<TargetNode>>;
    async fn owner(&self, file_set: &FileSet) -> anyhow::Result<TargetSet<TargetNode>>;
}

pub static NEW_BXL_CQUERY_FUNCTIONS: LateBinding<
    for<'c> fn(
        &'c DiceComputations,
        // Target platform
        Option<TargetLabel>,
        ProjectRoot,
        CellName,
    ) -> Pin<
        Box<dyn Future<Output = anyhow::Result<Box<dyn BxlCqueryFunctions<'c> + 'c>>> + 'c>,
    >,
> = LateBinding::new("NEW_BXL_CQUERY_FUNCTIONS");

pub static NEW_BXL_UQUERY_FUNCTIONS: LateBinding<
    for<'c> fn(
        &'c DiceComputations,
        ProjectRoot,
        CellName,
    ) -> Pin<
        Box<dyn Future<Output = anyhow::Result<Box<dyn BxlUqueryFunctions<'c> + 'c>>> + 'c>,
    >,
> = LateBinding::new("NEW_BXL_UQUERY_FUNCTIONS");
