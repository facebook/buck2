/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// Monitor the state of our execution and decide whether we should restart the command we just
/// attempted to execute.
pub struct Restarter {}

impl Restarter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn should_restart(&self) -> bool {
        false
    }
}
