/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::sync::Mutex;

use async_trait::async_trait;
use buck2_core::fs::project::ProjectRoot;
use buck2_events::dispatch::EventDispatcher;
use buck2_interpreter::starlark_debug::StarlarkDebugController;
use buck2_interpreter::starlark_debug::StarlarkDebuggerHandle;
use dupe::Dupe;
use tokio::sync::mpsc;

use crate::starlark_debug::error::StarlarkDebuggerError;
use crate::starlark_debug::run::ToClientMessage;
use crate::starlark_debug::server::BuckStarlarkDebuggerServer;

mod error;
pub mod run;
mod server;

mod dap_api;
/// A handle to the debugger server.
#[derive(Debug, Clone, Dupe)]
pub struct BuckStarlarkDebuggerHandle(Arc<HandleData>);

#[derive(Debug)]
pub struct HandleData {}

#[async_trait]
impl StarlarkDebuggerHandle for BuckStarlarkDebuggerHandle {
    async fn start_eval(
        &self,
        _description: &str,
    ) -> anyhow::Result<Box<dyn StarlarkDebugController>> {
        unimplemented!("coming soon")
    }
}

/// We allow only a single debugger to be attached at a time. While it's attached, this will hold the server instance.
static CURRENT_DEBUGGER: Mutex<Option<Arc<BuckStarlarkDebuggerServer>>> = Mutex::new(None);

/// Used by each command to get a handle to the current debugger. The debugger server will capture the
/// event dispatcher to send back debugger state snapshots (which indicate that the debugger is attached
/// and which, if any, threads are paused) while the command is running.
pub fn create_debugger_handle(events: EventDispatcher) -> Option<BuckStarlarkDebuggerHandle> {
    CURRENT_DEBUGGER
        .lock()
        .unwrap()
        .as_ref()
        .and_then(|v| v.new_handle(events))
}

/// Manages setting/unsetting the CURRENT_DEBUGGER while the starlark debug-attach command is running.
struct ServerConnection(Arc<BuckStarlarkDebuggerServer>);

impl ServerConnection {
    fn new(
        to_client_send: mpsc::UnboundedSender<ToClientMessage>,
        project_root: ProjectRoot,
    ) -> anyhow::Result<Self> {
        let mut locked = CURRENT_DEBUGGER.lock().unwrap();
        if locked.is_some() {
            return Err(StarlarkDebuggerError::DebuggerAlreadyAttached.into());
        }

        let server = Arc::new(BuckStarlarkDebuggerServer::new(
            to_client_send,
            project_root,
        ));
        *locked = Some(server.dupe());
        Ok(Self(server))
    }
}

impl Drop for ServerConnection {
    fn drop(&mut self) {
        *CURRENT_DEBUGGER.lock().unwrap() = None;
        let _ignored = self.0.detach();
    }
}
