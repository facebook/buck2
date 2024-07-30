/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::pin::Pin;
use std::sync::Arc;

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_build_api::deferred::types::DeferredRegistry;
use buck2_build_api::dynamic::lambda::DynamicLambda;
use buck2_core::base_deferred_key::BaseDeferredKeyDyn;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_execute::digest_config::DigestConfig;
use buck2_futures::cancellable_future::CancellationObserver;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use futures::Future;

pub static EVAL_BXL_FOR_DYNAMIC_OUTPUT: LateBinding<
    for<'v> fn(
        &'v Arc<dyn BaseDeferredKeyDyn>,
        &'v DynamicLambda,
        &'v mut DiceComputations,
        String,
        HashMap<ConfiguredTargetLabel, ConfiguredTargetNode>,
        HashMap<Artifact, ProjectRelativePathBuf>,
        &'v mut DeferredRegistry,
        ProjectRoot,
        DigestConfig,
        CancellationObserver,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<()>> + Send + 'v>>,
> = LateBinding::new("EVAL_BXL_FOR_DYNAMIC_OUTPUT");

pub(crate) async fn eval_bxl_for_dynamic_output<'v>(
    base_deferred_key: &'v Arc<dyn BaseDeferredKeyDyn>,
    dynamic_lambda: &'v DynamicLambda,
    dice_ctx: &'v mut DiceComputations<'_>,
    action_key: String,
    configured_targets: HashMap<ConfiguredTargetLabel, ConfiguredTargetNode>,
    materialized_artifacts: HashMap<Artifact, ProjectRelativePathBuf>,
    registry: &'v mut DeferredRegistry,
    project_filesystem: ProjectRoot,
    digest_config: DigestConfig,
    liveness: CancellationObserver,
) -> anyhow::Result<()> {
    (EVAL_BXL_FOR_DYNAMIC_OUTPUT.get()?)(
        base_deferred_key,
        dynamic_lambda,
        dice_ctx,
        action_key,
        configured_targets,
        materialized_artifacts,
        registry,
        project_filesystem,
        digest_config,
        liveness,
    )
    .await
}
