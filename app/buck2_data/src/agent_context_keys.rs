/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

impl crate::AgentContextEntry {
    /// Well-known agent_context keys injected from env vars.
    /// These bypass schema validation since they are not user-provided
    /// `--agent-context` fields.
    pub const KEY_ID: &'static str = "id";
    pub const KEY_INVOCATION_ID: &'static str = "invocation_id";
    pub const ENV_INJECTED_KEYS: &'static [&'static str] = &[Self::KEY_ID, Self::KEY_INVOCATION_ID];
}
