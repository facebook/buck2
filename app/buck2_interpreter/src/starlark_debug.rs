/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use starlark::eval::Evaluator;

/// A StarlarkDebuggerHandle is a reference to the global debugger server. It's used to get
/// a StarlarkDebugController when about to perform starlark evaluation.
#[async_trait]
pub trait StarlarkDebuggerHandle: Send + Sync + 'static {
    /// Indicates that we are about to start a starlark evaluation. This can be called multiple times on a handle.
    async fn start_eval(
        &self,
        description: &str,
    ) -> anyhow::Result<Box<dyn StarlarkDebugController>>;
}

/// The StarlarkDebugController is used to setup a starlark Evaluator for debugging.
pub trait StarlarkDebugController {
    /// Configures the Evaluator for debugging. Can only be used once.
    fn initialize(&mut self, eval: &mut Evaluator) -> anyhow::Result<()>;
}
