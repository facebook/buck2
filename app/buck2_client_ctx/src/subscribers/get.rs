/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::AtomicU64;
use std::sync::Arc;

use dupe::Dupe;

use crate::client_ctx::ClientCommandContext;
use crate::common::CommonEventLogOptions;
use crate::streaming::StreamingCommand;
use crate::subscribers::build_graph_stats::BuildGraphStats;
use crate::subscribers::build_id_writer::BuildIdWriter;
use crate::subscribers::event_log::EventLog;
use crate::subscribers::re_log::ReLog;
use crate::subscribers::subscriber::EventSubscriber;

/// Given the command arguments, conditionally create an event log.
pub(crate) fn try_get_event_log_subscriber<T: StreamingCommand>(
    cmd: &T,
    ctx: &ClientCommandContext,
    log_size_counter_bytes: Option<Arc<AtomicU64>>,
) -> buck2_error::Result<Option<Box<dyn EventSubscriber>>> {
    let event_log_opts = cmd.event_log_opts();
    let sanitized_argv = cmd.sanitize_argv(ctx.argv.clone());
    let user_event_log = cmd.user_event_log();

    if event_log_opts.no_event_log {
        return Ok(None);
    }
    let logdir = ctx.paths()?.log_dir();
    let log = EventLog::new(
        logdir,
        ctx.working_dir.clone(),
        event_log_opts
            .event_log
            .as_ref()
            .map(|p| p.resolve(&ctx.working_dir)),
        user_event_log.as_ref().map(|p| p.resolve(&ctx.working_dir)),
        sanitized_argv,
        T::COMMAND_NAME.to_owned(),
        log_size_counter_bytes,
    )?;
    Ok(Some(Box::new(log)))
}

pub(crate) fn try_get_re_log_subscriber(
    ctx: &ClientCommandContext,
) -> buck2_error::Result<Option<Box<dyn EventSubscriber>>> {
    let log = ReLog::new(ctx.paths()?.isolation.clone());
    Ok(Some(Box::new(log)))
}

pub(crate) fn try_get_build_id_writer(
    opts: &CommonEventLogOptions,
    ctx: &ClientCommandContext,
) -> buck2_error::Result<Option<Box<dyn EventSubscriber>>> {
    if let Some(file_loc) = opts.write_build_id.as_ref() {
        Ok(Some(Box::new(BuildIdWriter::new(
            file_loc.resolve(&ctx.working_dir),
        ))))
    } else {
        Ok(None)
    }
}

pub(crate) fn try_get_build_graph_stats<T: StreamingCommand>(
    cmd: &T,
    ctx: &ClientCommandContext,
) -> buck2_error::Result<Option<Box<dyn EventSubscriber>>> {
    if should_handle_build_graph_stats(cmd) {
        Ok(Some(Box::new(BuildGraphStats::new(
            ctx.fbinit(),
            ctx.trace_id.dupe(),
        ))))
    } else {
        Ok(None)
    }
}

fn should_handle_build_graph_stats<T: StreamingCommand>(cmd: &T) -> bool {
    // Currently, we only care about graph size info in BuildResponse which build command produces
    cmd.build_config_opts()
        .config_values
        .contains(&"buck2.log_configured_graph_size=true".to_owned())
        && cmd.logging_name() == "build"
}
