/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_cli_proto::new_generic::NewGenericRequest;
use buck2_cli_proto::new_generic::NewGenericResponse;

use crate::ctx::ServerCommandContext;
use crate::materialize::materialize_command;

pub(crate) async fn new_generic_command(
    context: &ServerCommandContext<'_>,
    req: buck2_cli_proto::NewGenericRequestMessage,
) -> anyhow::Result<buck2_cli_proto::NewGenericResponseMessage> {
    let req = req.new_generic_request;
    let req: NewGenericRequest =
        serde_json::from_str(&req).context("Could not deserialize `NewGenericRequest`")?;
    let resp = match req {
        NewGenericRequest::Materialize(m) => {
            NewGenericResponse::Materialize(materialize_command(context, m).await?)
        }
    };
    let resp = serde_json::to_string(&resp).context("Could not serialize `NewGenericResponse`")?;
    Ok(buck2_cli_proto::NewGenericResponseMessage {
        new_generic_response: resp,
    })
}
