/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::new_generic::ExpandExternalCellRequest;
use buck2_cli_proto::new_generic::NewGenericRequest;
use buck2_cli_proto::new_generic::NewGenericResponse;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;
use clap::ArgMatches;

/// Expand the contents of an external cell into the repo.
///
/// The contents are placed at the path you specified for this cell in your buckconfig.
///
/// If you additionally remove the entry from the `external_cells` section of your buckconfig, you
/// can edit the files directly in the repo and see those edits reflected in your build.
///
/// Note that this creates a point-in-time snapshot. The files in the repo will not be updated if
/// you eg change the git commit of the cell in the future.
#[derive(Debug, clap::Parser)]
#[clap(name = "expand-external-cell")]
pub struct ExpandExternalCellCommand {
    cell: String,
}

const REMINDER_TEXT: &str = "Reminder: For edits to the expanded cell to take effect on \
your build, you must additionally remove the entry from the `external_cells` section of your \
buckconfig";

#[async_trait::async_trait]
impl StreamingCommand for ExpandExternalCellCommand {
    const COMMAND_NAME: &'static str = "expand-external-cell";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let context = ctx.client_context(matches, &self)?;
        let resp = buckd
            .with_flushing()
            .new_generic(
                context,
                NewGenericRequest::ExpandExternalCell(ExpandExternalCellRequest {
                    cell_name: self.cell,
                }),
                None,
            )
            .await??;
        let NewGenericResponse::ExpandExternalCell(resp) = resp else {
            return ExitResult::bail("Unexpected response type from generic command");
        };

        let stdout = format!(
            "Expanded external cell to {}.\n\n{}",
            resp.path, REMINDER_TEXT,
        );

        ExitResult::success().with_stdout(stdout.into_bytes())
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::default_ref()
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        CommonEventLogOptions::default_ref()
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::default_ref()
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        CommonStarlarkOptions::default_ref()
    }
}
