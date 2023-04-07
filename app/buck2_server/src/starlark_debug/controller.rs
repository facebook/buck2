/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_interpreter::starlark_debug::StarlarkDebugController;
use dupe::Dupe;
use starlark::debug::DapAdapterEvalHook;
use tokio::sync::OwnedSemaphorePermit;
use tracing::debug;

use crate::starlark_debug::error::StarlarkDebuggerInternalError;
use crate::starlark_debug::server::BuckStarlarkDebuggerServer;
use crate::starlark_debug::HookId;

/// The debug controller is created once for each starlark evaluation. It adds the hooks to the
/// starlark Evaluator needed for debugging. Internally, the server will send and receive messages
/// with the DapAdapterEvalHooks that this injects to the Evaluator.
///
/// When evaluation finishes and the debug  controller is dropped, it will notify the server that it
/// has been dropped.
#[derive(Debug)]
pub struct BuckStarlarkDebugController {
    eval_wrapper: EvalWrapperHolder,
    server: Arc<BuckStarlarkDebuggerServer>,
    hook_id: HookId,
    description: String,
    _permit: OwnedSemaphorePermit,
    /// We don't want this object to be held across await points for a couple reasons: (1) it holds
    /// the semaphore permit and we want to limit the scope of where that needs to be held and (2) it's
    /// intended to wrap the starlark Evaluator which itself should not be sent across threads as it may
    /// hold a large starlark heap.
    _unsend: std::marker::PhantomData<*mut ()>,
}

impl StarlarkDebugController for BuckStarlarkDebugController {
    /// Initializes the Evaluator. This can only be used once for a particular controller.
    fn initialize(&mut self, eval: &mut starlark::eval::Evaluator) -> anyhow::Result<()> {
        match self.eval_wrapper.take()? {
            Some(v) => {
                debug!("adding dap hooks for {}", &self.description);
                v.add_dap_hooks(eval);
            }
            None => {
                // server is shutting down
            }
        };
        Ok(())
    }
}

impl BuckStarlarkDebugController {
    pub(crate) fn new(
        eval_wrapper: Option<Box<dyn DapAdapterEvalHook>>,
        hook_id: HookId,
        description: &str,
        server: &Arc<BuckStarlarkDebuggerServer>,
        permit: tokio::sync::OwnedSemaphorePermit,
    ) -> Self {
        Self {
            eval_wrapper: match eval_wrapper {
                Some(v) => EvalWrapperHolder::Ready(v),
                None => EvalWrapperHolder::ShuttingDown,
            },
            hook_id,
            description: description.to_owned(),
            server: server.dupe(),
            _permit: permit,
            _unsend: std::marker::PhantomData,
        }
    }
}

impl Drop for BuckStarlarkDebugController {
    fn drop(&mut self) {
        self.server.drop_hook(self.hook_id)
    }
}

#[derive(Debug)]
enum EvalWrapperHolder {
    /// The normal case, we have the eval hook ready to initialize an Evaluator.
    Ready(Box<dyn DapAdapterEvalHook>),
    /// Once initialize() has been called we'll change to the Used state (in which it'll be an
    /// error to initialize again).
    Used,
    /// Indicates that this was created while the debug server was already shutting down. That's
    /// an expected case and we just ignore it and don't add hooks to the Evaluator in that case.
    ShuttingDown,
}

impl EvalWrapperHolder {
    fn take(&mut self) -> anyhow::Result<Option<Box<dyn DapAdapterEvalHook>>> {
        match self {
            EvalWrapperHolder::ShuttingDown => Ok(None),
            EvalWrapperHolder::Used => {
                Err(StarlarkDebuggerInternalError::EvalWrapperAlreadyUsed.into())
            }
            EvalWrapperHolder::Ready(_) => {
                let owned = std::mem::replace(self, EvalWrapperHolder::Used);
                match owned {
                    EvalWrapperHolder::Ready(v) => Ok(Some(v)),
                    _ => unreachable!(),
                }
            }
        }
    }
}
