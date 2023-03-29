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
use buck2_build_api::bxl::types::BxlKey;
use buck2_cli_proto::build_request::Materializations;
use buck2_cli_proto::profile_request::ProfileOpts;
use buck2_cli_proto::ProfileRequest;
use buck2_cli_proto::ProfileResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_interpreter::starlark_profiler::StarlarkProfileModeOrInstrumentation;
use buck2_profile::get_profile_response;
use buck2_profile::starlark_profiler_configuration_from_request;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;

use crate::bxl::eval::eval;
use crate::bxl::eval::BxlResolvedCliArgs;
use crate::command::get_bxl_cli_args;
use crate::command::parse_bxl_label_from_cli;

pub async fn bxl_profile_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: ProfileRequest,
) -> anyhow::Result<ProfileResponse> {
    run_server_command(
        BxlProfileServerCommand { req },
        ctx,
        partial_result_dispatcher,
    )
    .await
}

struct BxlProfileServerCommand {
    req: ProfileRequest,
}

#[async_trait]
impl ServerCommandTemplate for BxlProfileServerCommand {
    type StartEvent = buck2_data::ProfileCommandStart;
    type EndEvent = buck2_data::ProfileCommandEnd;
    type Response = buck2_cli_proto::ProfileResponse;
    type PartialResult = NoPartialResult;

    async fn command<'v>(
        &self,
        server_ctx: &'v dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
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

                        let cell_resolver = ctx.get_cell_resolver().await?;
                        let bxl_label =
                            parse_bxl_label_from_cli(cwd, &opts.bxl_label, &cell_resolver)?;

                        let bxl_args = match get_bxl_cli_args(
                            cwd,
                            &ctx,
                            &bxl_label,
                            &opts.bxl_args,
                            &cell_resolver,
                        )
                        .await?
                        {
                            BxlResolvedCliArgs::Resolved(bxl_args) => Arc::new(bxl_args),
                            _ => {
                                return Err(anyhow::anyhow!(
                                    "Help docs were displayed. No profiler data available"
                                ));
                            }
                        };

                        let global_target_platform = target_platform_from_client_context(
                            self.req.context.as_ref(),
                            &cell_resolver,
                            cwd,
                        )
                        .await?;

                        let bxl_key = BxlKey::new(
                            bxl_label.clone(),
                            bxl_args,
                            global_target_platform,
                            Materializations::Skip,
                        );

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
