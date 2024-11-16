/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use core::iter::Iterator;

use buck2_cli_proto::new_generic::ExplainRequest;
use buck2_core::pattern::pattern_type::ConfiguredTargetPatternExtra;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_query::query::syntax::simple::eval::label_indexed::LabelIndexedSet;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::pattern_parse_and_resolve::parse_and_resolve_patterns_to_targets_from_cli_args;
use buck2_server_ctx::target_resolution_config::TargetResolutionConfig;
use dice::DiceTransaction;
use dupe::Dupe;
use dupe::IterDupedExt;

pub(crate) async fn explain(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    req: &ExplainRequest,
) -> anyhow::Result<()> {
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

    buck2_explain::main(
        all_deps,
        req.output.as_ref(),
        req.fbs_dump.as_ref(),
        req.manifold_path.as_deref(),
    )
    .await?;

    Ok(())
}
