/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::new_generic::CompleteRequest;
use buck2_cli_proto::new_generic::CompleteResponse;
use buck2_common::pattern::parse_from_cli::parse_patterns_from_cli_args;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::load_patterns::load_patterns;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;

pub(crate) async fn complete_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: CompleteRequest,
) -> buck2_error::Result<CompleteResponse> {
    run_server_command(
        CompleteServerCommand { req },
        ctx,
        partial_result_dispatcher,
    )
    .await
}

struct CompleteServerCommand {
    req: CompleteRequest,
}

#[async_trait::async_trait]
impl ServerCommandTemplate for CompleteServerCommand {
    type StartEvent = buck2_data::CompleteCommandStart;
    type EndEvent = buck2_data::CompleteCommandEnd;
    type Response = buck2_cli_proto::new_generic::CompleteResponse;
    type PartialResult = NoPartialResult;

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        mut dice: DiceTransaction,
    ) -> buck2_error::Result<Self::Response> {
        let cwd = server_ctx.working_dir();
        let parsed_target_patterns = parse_patterns_from_cli_args::<TargetPatternExtra>(
            &mut dice,
            &[self.req.partial_target.clone()],
            cwd,
        )
        .await?;

        let results = &load_patterns(
            &mut dice,
            parsed_target_patterns,
            MissingTargetBehavior::Fail,
        )
        .await?;

        let mut output: Vec<String> = vec![];
        for node in results.iter_loaded_targets() {
            let node = node?;

            // FIXME(JakobDegen): This is kind of a hack, we shouldn't really be inspecting
            // attribute we know nothing about. It's also pretty difficult to fix this right now
            // though.
            if let Some(labels) = node.attr_or_none("labels", AttrInspectOptions::All) {
                if let CoercedAttr::List(labels) = labels.value {
                    if labels.iter().any(|label| match label {
                        CoercedAttr::String(label) => ***label == *"generated",
                        _ => false,
                    }) {
                        // Skip generated targets
                        continue;
                    }
                }
            }
            output.push(format!("{}", node.label()));
        }
        Ok(CompleteResponse {
            completions: output,
        })
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        true
    }

    fn exclusive_command_name(&self) -> Option<String> {
        Some("complete".to_owned())
    }
}
