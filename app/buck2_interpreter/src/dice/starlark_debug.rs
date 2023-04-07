/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use dice::DiceComputations;
use dice::UserComputationData;

use crate::starlark_debug::StarlarkDebuggerHandle;

pub trait HasStarlarkDebugger {
    fn get_starlark_debugger_handle(&self) -> Option<&dyn StarlarkDebuggerHandle>;
}

pub trait SetStarlarkDebugger {
    fn set_starlark_debugger_handle(&mut self, hook: Option<impl StarlarkDebuggerHandle>);
}

impl HasStarlarkDebugger for DiceComputations {
    fn get_starlark_debugger_handle(&self) -> Option<&dyn StarlarkDebuggerHandle> {
        self.per_transaction_data()
            .data
            .get::<StarlarkDebuggerHookHolder>()
            .expect("Starlark debugger hook should be set")
            .hook
            .as_deref()
    }
}

impl SetStarlarkDebugger for UserComputationData {
    fn set_starlark_debugger_handle(&mut self, hook: Option<impl StarlarkDebuggerHandle>) {
        self.data.set(StarlarkDebuggerHookHolder {
            hook: hook.map(|v| Box::new(v) as _),
        })
    }
}

struct StarlarkDebuggerHookHolder {
    hook: Option<Box<dyn StarlarkDebuggerHandle>>,
}
