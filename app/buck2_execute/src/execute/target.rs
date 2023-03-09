/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use derivative::Derivative;
use dupe::Dupe;

pub trait CommandExecutionTargetImpl: Send + Sync + Debug {
    fn re_action_key(&self) -> String;

    fn re_affinity_key(&self) -> String;

    fn as_proto_action_key(&self) -> buck2_data::ActionKey;

    fn as_proto_action_name(&self) -> buck2_data::ActionName;
}

// FIXME: Remove this type entirely.
/// Indicates why we are executing a given command.
#[derive(Clone, Dupe, Derivative)]
#[derivative(Debug)]
pub struct CommandExecutionTarget<'a> {
    inner: &'a dyn CommandExecutionTargetImpl,
}

impl<'a> CommandExecutionTarget<'a> {
    pub fn new(inner: &'a dyn CommandExecutionTargetImpl) -> Self {
        Self { inner }
    }

    pub fn re_action_key(&self) -> String {
        self.inner.re_action_key()
    }

    pub fn re_affinity_key(&self) -> String {
        self.inner.re_affinity_key()
    }

    pub fn as_proto_action_key(&self) -> buck2_data::ActionKey {
        self.inner.as_proto_action_key()
    }

    pub fn as_proto_action_name(&self) -> buck2_data::ActionName {
        self.inner.as_proto_action_name()
    }
}
