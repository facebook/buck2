/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsString;
use std::path::PathBuf;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::replayer::Replayer;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::working_dir::WorkingDir;
use tokio::runtime::Runtime;

use crate::commands::debug::ExecFn;
use crate::commands::log::options::EventLogOptions;

#[derive(Debug, clap::Parser)]
#[clap(
    setting = clap::AppSettings::TrailingVarArg
)]
pub struct ReplayCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,

    #[clap(
        long,
        help = "Control the playback speed using a float (i.e. 0.5, 2, etc)",
        value_name = "NUMBER"
    )]
    pub speed: Option<f64>,

    /// Preload the event log. This is typically only useful for benchmarking.
    #[clap(long)]
    preload: bool,

    #[clap(help = "Override the arguments")]
    pub override_args: Vec<String>,
}

impl ReplayCommand {
    pub fn exec(
        self,
        _matches: &clap::ArgMatches,
        ctx: ClientCommandContext,
        exec: impl ExecFn,
    ) -> ExitResult {
        let Self {
            event_log,
            speed,
            preload,
            mut override_args,
        } = self;

        let runtime = Runtime::new().expect("Should be able to start a runtime");
        let (replayer, invocation) =
            runtime.block_on(Replayer::new(event_log.get(&ctx)?, speed, preload))?;

        let (args, working_dir) = if override_args.is_empty() {
            (
                invocation.command_line_args,
                WorkingDir::unchecked_new(AbsNormPathBuf::new(PathBuf::from(OsString::from(
                    invocation.working_dir,
                )))?),
            )
        } else {
            override_args.insert(0, "buck2".to_owned());
            (override_args, ctx.working_dir.clone())
        };

        exec(
            args,
            working_dir,
            ctx.process_context,
            replayer,
            invocation.trace_id,
        )
    }
}
