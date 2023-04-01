/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Write;
use std::iter;

use async_trait::async_trait;
use buck2_build_api::calculation::MissingTargetBehavior;
use buck2_build_api::configure_targets::load_compatible_patterns;
use buck2_cli_proto::ConfiguredTargetsRequest;
use buck2_cli_proto::ConfiguredTargetsResponse;
use buck2_cli_proto::HasClientContext;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;

use crate::commands::targets::fmt::print_target_call_stack_after_target;

pub async fn configured_targets_command(
    server_ctx: Box<dyn ServerCommandContextTrait>,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: ConfiguredTargetsRequest,
) -> anyhow::Result<ConfiguredTargetsResponse> {
    run_server_command(
        ConfiguredTargetsServerCommand { req },
        server_ctx,
        partial_result_dispatcher,
    )
    .await
}

struct ConfiguredTargetsServerCommand {
    req: ConfiguredTargetsRequest,
}

#[async_trait]
impl ServerCommandTemplate for ConfiguredTargetsServerCommand {
    type StartEvent = buck2_data::ConfiguredTargetsCommandStart;
    type EndEvent = buck2_data::ConfiguredTargetsCommandEnd;
    type Response = ConfiguredTargetsResponse;
    type PartialResult = NoPartialResult;

    fn is_success(&self, response: &ConfiguredTargetsResponse) -> bool {
        let ConfiguredTargetsResponse {
            serialized_targets_output,
        } = response;
        let _ignore = serialized_targets_output;
        true
    }

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<ConfiguredTargetsResponse> {
        // TODO(nga): this should accept `ConfiguredTargetPatternExtra`. And handle the universe.
        let parsed_patterns = parse_patterns_from_cli_args::<TargetPatternExtra>(
            &ctx,
            &self.req.target_patterns,
            server_ctx.working_dir(),
        )
        .await?;

        let client_ctx = self.req.client_context()?;
        let global_target_platform =
            target_platform_from_client_context(client_ctx, server_ctx, &ctx).await?;

        let skip_missing_targets = MissingTargetBehavior::from_skip(self.req.skip_missing_targets);

        let compatible_targets = load_compatible_patterns(
            &ctx,
            parsed_patterns,
            global_target_platform,
            skip_missing_targets,
        )
        .await?;

        let mut serialized_targets_output = String::new();
        for node in compatible_targets.into_iter() {
            // TODO(nga): we should probably get rid of forward nodes.
            let nodes = iter::once(&node).chain(node.forward_target());

            for node in nodes {
                writeln!(serialized_targets_output, "{}", node.label())?;
                if self.req.target_call_stacks {
                    print_target_call_stack_after_target(
                        &mut serialized_targets_output,
                        node.call_stack().as_deref(),
                    );
                }
            }
        }

        Ok(ConfiguredTargetsResponse {
            serialized_targets_output,
        })
    }
}
