/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_client_ctx::restarter::Restarter;
use buck2_client_ctx::stdin::Stdin;
use buck2_core::fs::working_dir::WorkingDir;
use buck2_core::logging::LogConfigurationReloadHandle;

/// State passed down from `main` to this crate.
pub struct ProcessContext<'a> {
    pub init: fbinit::FacebookInit,
    pub log_reload_handle: &'a Arc<dyn LogConfigurationReloadHandle>,
    pub stdin: &'a mut Stdin,
    pub working_dir: &'a WorkingDir,
    pub args: &'a [String],
    pub restarter: &'a mut Restarter,
    pub force_new_trace_id: bool,
}
