/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::execute::prepared::ActionPaths;
use crate::execute::target::CommandExecutionTarget;

pub struct ReActionIdentity<'a, 'b> {
    /// This is currently unused, but historically it has been useful to add logging in the RE
    /// client, so it's worth keeping around.
    _target: &'a CommandExecutionTarget<'b>,

    /// Actions with the same action key share e.g. memory requirements learnt by RE.
    pub action_key: String,

    /// Actions with the same affinity key get scheduled on similar hosts.
    pub affinity_key: String,

    /// Details about the action collected while uploading
    pub action_paths: &'a ActionPaths,
}

impl<'a, 'b> ReActionIdentity<'a, 'b> {
    pub fn new(
        target: &'a CommandExecutionTarget<'b>,
        executor_action_key: Option<&str>,
        action_paths: &'a ActionPaths,
    ) -> Self {
        let mut action_key = target.re_action_key();
        if let Some(executor_action_key) = executor_action_key {
            action_key = format!("{} {}", executor_action_key, action_key);
        }

        Self {
            _target: target,
            action_key,
            affinity_key: target.re_affinity_key(),
            action_paths,
        }
    }
}
