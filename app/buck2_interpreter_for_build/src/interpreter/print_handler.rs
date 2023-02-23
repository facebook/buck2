/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_events::dispatch::EventDispatcher;
use starlark::PrintHandler;

/// Print handler uses the `EventDispatcher` to emit messages from server to client.
pub struct EventDispatcherPrintHandler(pub EventDispatcher);

impl PrintHandler for EventDispatcherPrintHandler {
    fn println(&self, text: &str) -> anyhow::Result<()> {
        self.0.console_message(text.to_owned());
        Ok(())
    }
}
