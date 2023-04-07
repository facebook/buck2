/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::fs::project::ProjectRoot;
use buck2_events::dispatch::EventDispatcher;
use debugserver_types as dap;
use tokio::sync::mpsc;

use crate::starlark_debug::run::ToClientMessage;
use crate::starlark_debug::BuckStarlarkDebuggerHandle;

/// The buck starlark debugger server. Most of the work is managed by the single-threaded server state.
///
/// There will be several references to the BuckStarlarkDebuggerServer instance and it will forward messages
/// along to the state.
#[derive(Debug)]
pub(crate) struct BuckStarlarkDebuggerServer {}

impl BuckStarlarkDebuggerServer {
    pub(crate) fn new(
        _to_client: mpsc::UnboundedSender<ToClientMessage>,
        _project_root: ProjectRoot,
    ) -> Self {
        Self {}
    }
    pub(crate) fn new_handle(
        self: &Arc<Self>,
        _events: EventDispatcher,
    ) -> Option<BuckStarlarkDebuggerHandle> {
        unimplemented!("coming soon")
    }

    /// Called to forward along requests from the DAP client.
    pub(crate) fn send_request(&self, _req: dap::Request) -> anyhow::Result<()> {
        unimplemented!("coming soon")
    }

    /// Called when the DAP client has disconnected.
    pub(crate) fn detach(&self) -> anyhow::Result<()> {
        Ok(())
    }
}
