/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::new_generic::ExpandExternalCellsRequest;
use buck2_cli_proto::new_generic::NewGenericRequest;
use buck2_cli_proto::new_generic::NewGenericResponse;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;

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
pub struct ExpandExternalCellsCommand {
    /// Expand all cells that Buck2 knows about
    #[clap(long, conflicts_with = "cells")]
    all_cells: bool,

    cells: Vec<String>,
}

const REMINDER_TEXT: &str = "Reminder: For edits to the expanded cell to take effect on \
your build, you must additionally remove the entry from the `external_cells` section of your \
buckconfig";

#[async_trait::async_trait]
impl StreamingCommand for ExpandExternalCellsCommand {
    const COMMAND_NAME: &'static str = "expand-external-cell";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: BuckArgMatches<'_>,
        ctx: &mut ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let context = ctx.client_context(matches, &self)?;
        let req = if self.all_cells {
            ExpandExternalCellsRequest::All
        } else {
            ExpandExternalCellsRequest::Specific(self.cells.into_iter().collect())
        };
        let resp = buckd
            .with_flushing()
            .new_generic(
                context,
                NewGenericRequest::ExpandExternalCells(req),
                events_ctx,
                None,
            )
            .await??;
        let NewGenericResponse::ExpandExternalCells(resp) = resp else {
            return ExitResult::bail("Unexpected response type from generic command");
        };

        let mut lines: Vec<String> = resp
            .paths
            .into_iter()
            .map(|(cell, path)| format!("Expanded external cell {} to {}.", cell, path))
            .collect();
        lines.push(String::new());
        lines.push(REMINDER_TEXT.to_owned());

        ExitResult::success().with_stdout(lines.join("\n").into_bytes())
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
