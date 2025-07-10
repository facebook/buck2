/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_client_ctx::restarter::Restarter;
use buck2_client_ctx::stdin::Stdin;
use buck2_core::fs::working_dir::AbsWorkingDir;
use buck2_core::logging::LogConfigurationReloadHandle;
use buck2_wrapper_common::invocation_id::TraceId;

/// State passed down from `main` to this crate.
pub struct ProcessContext<'a> {
    pub start_time: u64,
    pub trace_id: TraceId,
    /// An invocation that this invocation is a restart of.
    pub restarted_trace_id: Option<TraceId>,
    pub shared: &'a mut SharedProcessContext,
}

// Process context shared with restarted commands.
pub struct SharedProcessContext {
    pub log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,
    pub stdin: Stdin,
    pub working_dir: AbsWorkingDir,
    pub args: Vec<String>,
    pub restarter: Restarter,
    pub force_want_restart: bool,
}

impl<'a> ProcessContext<'a> {
    pub fn new(
        start_time: u64,
        trace_id: TraceId,
        restarted_trace_id: Option<TraceId>,
        shared: &'a mut SharedProcessContext,
    ) -> Self {
        Self {
            start_time,
            trace_id,
            restarted_trace_id,
            shared,
        }
    }
}
