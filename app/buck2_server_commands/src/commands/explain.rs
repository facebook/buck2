/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::new_generic::ExplainRequest;
use buck2_cli_proto::new_generic::ExplainResponse;
use buck2_core::pattern::pattern_type::ConfiguredTargetPatternExtra;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_query::query::syntax::simple::eval::label_indexed::LabelIndexedSet;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern_parse_and_resolve::parse_and_resolve_patterns_to_targets_from_cli_args;
use buck2_server_ctx::target_resolution_config::TargetResolutionConfig;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;
use dupe::Dupe;
use dupe::IterDupedExt;
use tonic::async_trait;

pub(crate) async fn explain_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: ExplainRequest,
) -> anyhow::Result<ExplainResponse> {
    run_server_command(ExplainServerCommand { req }, ctx, partial_result_dispatcher).await
}

struct ExplainServerCommand {
    req: ExplainRequest,
}

#[async_trait]
impl ServerCommandTemplate for ExplainServerCommand {
    type StartEvent = buck2_data::ExplainCommandStart;
    type EndEvent = buck2_data::ExplainCommandEnd;
    type Response = buck2_cli_proto::new_generic::ExplainResponse;
    type PartialResult = NoPartialResult;

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        explain(server_ctx, ctx, &self.req).await
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }

    fn exclusive_command_name(&self) -> Option<String> {
        Some("explain".to_owned())
    }
}

pub(crate) async fn explain(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    req: &ExplainRequest,
) -> anyhow::Result<ExplainResponse> {
    let configured_target = {
        // TODO iguridi: this is hacky
        let target_pattern = parse_and_resolve_patterns_to_targets_from_cli_args::<
            ConfiguredTargetPatternExtra,
        >(&mut ctx, &[req.target.clone()], server_ctx.working_dir())
        .await?;

        let target_label = match target_pattern.as_slice() {
            [p] => &p.target_label,
            _ => {
                return Err(anyhow::anyhow!(
                    "Expected exactly one target, got {}",
                    target_pattern.len()
                ));
            }
        };

        let target_resolution_config = TargetResolutionConfig::from_args(
            &mut ctx,
            &req.target_cfg,
            server_ctx,
            &req.target_universe,
        )
        .await?;

        let configured_targets = target_resolution_config
            .get_configured_target(&mut ctx, target_label)
            .await?;
        if configured_targets.len() != 1 {
            return Err(anyhow::anyhow!(
                "Expected exactly one target, got {}",
                configured_targets.len()
            ));
        }
        ctx.get_configured_target_node(&configured_targets[0])
            .await?
            .require_compatible()? // TODO iguridi: not sure about this, make things simpler for now
    };

    let all_deps = {
        let mut stack = vec![configured_target];
        let mut visited = LabelIndexedSet::new();
        while let Some(node) = stack.pop() {
            if visited.insert(node.dupe()) {
                stack.extend(node.deps().duped());
            }
        }
        visited.into_iter().collect::<Vec<ConfiguredTargetNode>>()
    };

    // TODO iguridi: make it work for OSS
    #[cfg(fbcode_build)]
    {
        buck2_explain::main(
            all_deps,
            req.output.as_ref(),
            req.fbs_dump.as_ref(),
            req.manifold_path.as_deref(),
        )
        .await?;
    }
    #[cfg(not(fbcode_build))]
    {
        // just "using" unused variables
        let _all_deps = all_deps;
    }

    Ok(ExplainResponse {})
}
