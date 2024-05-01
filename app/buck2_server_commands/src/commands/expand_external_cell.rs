/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::new_generic::ExpandExternalCellRequest;
use buck2_cli_proto::new_generic::ExpandExternalCellResponse;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;

pub(crate) async fn expand_external_cell_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: ExpandExternalCellRequest,
) -> anyhow::Result<ExpandExternalCellResponse> {
    run_server_command(
        ExpandExternalCellServerCommand { req },
        ctx,
        partial_result_dispatcher,
    )
    .await
}

struct ExpandExternalCellServerCommand {
    req: ExpandExternalCellRequest,
}

#[async_trait::async_trait]
impl ServerCommandTemplate for ExpandExternalCellServerCommand {
    type StartEvent = buck2_data::ExpandExternalCellCommandStart;
    type EndEvent = buck2_data::ExpandExternalCellCommandEnd;
    type Response = buck2_cli_proto::new_generic::ExpandExternalCellResponse;
    type PartialResult = NoPartialResult;

    async fn command(
        &self,
        _server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        _ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        let _unused = &self.req;
        todo!() // TODO(JakobDegen): Implement in next diff
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        true
    }

    fn exclusive_command_name(&self) -> Option<String> {
        Some("expand-external-cell".to_owned())
    }
}
