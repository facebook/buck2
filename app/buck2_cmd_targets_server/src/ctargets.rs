/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::iter;

use async_trait::async_trait;
use buck2_build_api::configure_targets::load_compatible_patterns_with_modifiers;
use buck2_cli_proto::ConfiguredTargetsRequest;
use buck2_cli_proto::ConfiguredTargetsResponse;
use buck2_common::pattern::parse_from_cli::parse_patterns_with_modifiers_from_cli_args;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_error::BuckErrorContext;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::global_cfg_options::global_cfg_options_from_client_context;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::template::ServerCommandTemplate;
use buck2_server_ctx::template::run_server_command;
use dice::DiceTransaction;

use crate::targets::fmt::create_configured_formatter;

pub async fn configured_targets_command(
    server_ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: ConfiguredTargetsRequest,
) -> buck2_error::Result<ConfiguredTargetsResponse> {
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

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        mut ctx: DiceTransaction,
    ) -> buck2_error::Result<ConfiguredTargetsResponse> {
        // TODO(nga): this should accept `ConfiguredTargetPatternExtra`. And handle the universe.
        let parsed_patterns_with_modifiers =
            parse_patterns_with_modifiers_from_cli_args::<TargetPatternExtra>(
                &mut ctx,
                &self.req.target_patterns,
                server_ctx.working_dir(),
            )
            .await?;

        let formatter = create_configured_formatter(&self.req)?;

        let global_cfg_options = global_cfg_options_from_client_context(
            self.req
                .target_cfg
                .as_ref()
                .internal_error("target_cfg must be set")?,
            server_ctx,
            &mut ctx,
        )
        .await?;

        let skip_missing_targets = MissingTargetBehavior::from_skip(self.req.skip_missing_targets);

        let compatible_targets = load_compatible_patterns_with_modifiers(
            &mut ctx,
            parsed_patterns_with_modifiers,
            &global_cfg_options,
            skip_missing_targets,
        )
        .await?;

        let mut serialized_targets_output = String::new();
        let mut needs_separator = false;

        formatter.begin(&mut serialized_targets_output);
        for node in compatible_targets.into_iter() {
            // TODO(nga): we should probably get rid of forward nodes.
            let nodes = iter::once(&node).chain(node.forward_target());

            for node in nodes {
                if needs_separator {
                    formatter.separator(&mut serialized_targets_output);
                }
                needs_separator = true;
                formatter.target(node, &mut serialized_targets_output)?;
            }
        }
        formatter.end(&mut serialized_targets_output);

        Ok(ConfiguredTargetsResponse {
            serialized_targets_output,
        })
    }
}
