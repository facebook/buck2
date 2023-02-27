/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(unused)]

use buck2_core::fs::project_rel_path::ProjectRelativePath;

/// Subscriptions allow clients to request eager materialization of specific paths as well as
/// notifications when those paths are materialized.
pub struct MaterializerSubscriptions {}

impl MaterializerSubscriptions {
    pub fn new() -> Self {
        Self {}
    }

    /// Return whether a given path should be materialized eagerly.
    pub fn should_materialize_eagerly(&self, path: &ProjectRelativePath) -> bool {
        false
    }

    /// Notify this subscription that a given path has been materialized.
    pub fn on_materialization_finished(&self, path: &ProjectRelativePath) {}
}
