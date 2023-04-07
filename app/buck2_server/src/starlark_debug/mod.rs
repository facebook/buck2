/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_events::dispatch::EventDispatcher;
use buck2_interpreter::starlark_debug::StarlarkDebugController;
use buck2_interpreter::starlark_debug::StarlarkDebuggerHandle;
use dupe::Dupe;

mod error;
pub mod run;

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
/// Used by each command to get a handle to the current debugger. The debugger server will capture the
/// event dispatcher to send back debugger state snapshots (which indicate that the debugger is attached
/// and which, if any, threads are paused) while the command is running.
pub fn create_debugger_handle(_events: EventDispatcher) -> Option<BuckStarlarkDebuggerHandle> {
    // We don't yet support a way to set the global debugger, so return no handle.
    None
}
