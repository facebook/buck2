/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_client_ctx::client_ctx::BuckSubcommand;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_error::BuckErrorContext;
use buck2_fs::fs_util::uncategorized as fs_util;
use buck2_wrapper_common::KillallFilter;
use buck2_wrapper_common::is_buck2::WhoIsAsking;

/// Kill all buck2 processes on the machine
///
/// By default this kills every buck2 process regardless of repository or isolation dir.
/// Passing `--in-isolation-dir` narrows the kill to processes using that isolation dir, and
/// `--repo` narrows it to processes running in the current repository. Processes that
/// cannot be checked against the requested filter are skipped and reported.
#[derive(Debug, clap::Parser)]
#[clap(verbatim_doc_comment)]
pub struct KillallCommand {
    /// Only kill buck2 processes using this isolation dir.
    ///
    /// Unlike the global `--isolation-dir` flag, this does not default to `v2`; when
    /// omitted, processes of every isolation dir are killed.
    #[clap(long, value_name = "ISOLATION_DIR")]
    in_isolation_dir: Option<String>,

    /// Only kill buck2 processes running in the current repository (project root).
    #[clap(long)]
    repo: bool,

    #[clap(flatten)]
    pub(crate) event_log_opts: CommonEventLogOptions,
}

impl BuckSubcommand for KillallCommand {
    const COMMAND_NAME: &'static str = "killall";

    async fn exec_impl(
        self,
        _matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
        _events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let project_root = self
            .repo
            .then(|| {
                // Process working directories are read fully resolved from the OS, so
                // canonicalize the root to make the comparison symlink-insensitive.
                let paths = ctx
                    .paths()
                    .buck_error_context("`--repo` requires running from within a repository")?;
                buck2_error::Ok(fs_util::canonicalize(paths.project_root().root())?.into_path_buf())
            })
            .transpose()?;

        let filter = KillallFilter {
            isolation_dir: self.in_isolation_dir,
            project_root,
        };

        buck2_wrapper_common::killall(WhoIsAsking::Buck2, &filter, |s| {
            let _ignored = buck2_client_ctx::eprintln!("{}", s);
        })
        .then_some(())
        .ok_or(buck2_error::buck2_error!(
            buck2_error::ErrorTag::KillAll,
            "Killall command failed"
        ))
        .into()
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }
}
