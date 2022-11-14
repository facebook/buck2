/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_interpreter::starlark_profiler::StarlarkProfileModeOrInstrumentation;
use buck2_profile::get_profile_response;
use buck2_profile::starlark_profiler_configuration_from_request;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use cli_proto::profile_request::ProfileOpts;
use cli_proto::ProfileRequest;
use cli_proto::ProfileResponse;
use dice::DiceTransaction;

use crate::bxl::eval::eval;
use crate::command::get_bxl_key;

pub async fn bxl_profile_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    req: ProfileRequest,
) -> anyhow::Result<ProfileResponse> {
    run_server_command(BxlProfileServerCommand { req }, ctx).await
}

struct BxlProfileServerCommand {
    req: ProfileRequest,
}

#[async_trait]
impl ServerCommandTemplate for BxlProfileServerCommand {
    type StartEvent = buck2_data::ProfileCommandStart;
    type EndEvent = buck2_data::ProfileCommandEnd;
    type Response = cli_proto::ProfileResponse;

    async fn command(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        match self
            .req
            .profile_opts
            .as_ref()
            .expect("BXL profile opts not populated")
        {
            ProfileOpts::BxlProfile(opts) => {
                let output: PathBuf = self.req.destination_path.clone().into();

                let profile_mode = starlark_profiler_configuration_from_request(&self.req)?;

                let profile_data = match profile_mode {
                    StarlarkProfilerConfiguration::ProfileBxl(profile_mode) => {
                        let cwd = server_ctx.working_dir();

                        let bxl_key =
                            get_bxl_key(cwd, &ctx, &opts.bxl_label, &opts.bxl_args).await?;
                        eval(
                            ctx,
                            bxl_key,
                            StarlarkProfileModeOrInstrumentation::Profile(profile_mode),
                        )
                        .await?
                        .1
                        .map(Arc::new)
                        .expect("No bxl profile data found")
                    }
                    _ => {
                        return Err(anyhow::anyhow!("Incorrect profile mode (internal error)"));
                    }
                };

                get_profile_response(profile_data, &self.req, output)
            }
            _ => {
                return Err(anyhow::anyhow!(
                    "Expected BXL profile opts, not target profile opts"
                ));
            }
        }
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }
}
