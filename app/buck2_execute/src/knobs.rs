/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use dupe::Dupe;

/// Command-level config that can tweak how the executors work.
#[derive(Clone, Dupe, Default)]
pub struct ExecutorGlobalKnobs {
    pub enable_miniperf: bool,

    /// Whether to emit action keys to execution logs (thos are pretty verbose and omitted by
    /// default).
    pub log_action_keys: bool,
}
