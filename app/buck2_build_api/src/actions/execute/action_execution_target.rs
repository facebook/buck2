/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Write;

use buck2_core::category::CategoryRef;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::fs::buck_out_path::BuckOutScratchPath;
use buck2_data::ToProtoMessage;
use buck2_execute::execute::target::CommandExecutionTarget;
use derivative::Derivative;
use dupe::Dupe;

use crate::actions::RegisteredAction;

/// Indicates why we are executing a given command.
#[derive(Clone, Dupe, Derivative)]
#[derivative(Debug)]
pub struct ActionExecutionTarget<'a> {
    action: &'a RegisteredAction,
}

impl<'a> ActionExecutionTarget<'a> {
    pub(crate) fn new(action: &'a RegisteredAction) -> Self {
        ActionExecutionTarget { action }
    }

    pub fn owner(&self) -> &'a BaseDeferredKey {
        self.action.owner()
    }

    pub fn category(&self) -> CategoryRef<'a> {
        self.action.category()
    }

    pub fn identifier(&self) -> Option<&'a str> {
        self.action.identifier()
    }

    pub fn scratch_path(&self) -> BuckOutScratchPath {
        BuckOutScratchPath::new(
            self.action.owner().dupe(),
            self.action.category(),
            self.action.identifier(),
            self.action.action_key(),
        )
        .unwrap()
    }
}

impl CommandExecutionTarget for ActionExecutionTarget<'_> {
    fn re_action_key(&self) -> String {
        let mut key = String::new();
        write!(
            &mut key,
            "{} {}",
            self.action.owner(),
            self.action.category()
        )
        .unwrap();
        if let Some(ident) = self.action.identifier().as_ref() {
            write!(&mut key, " {}", ident).unwrap();
        }
        key
    }

    fn re_affinity_key(&self) -> String {
        self.action.owner().to_string()
    }

    fn as_proto_action_key(&self) -> buck2_data::ActionKey {
        self.action.key().as_proto()
    }

    fn as_proto_action_name(&self) -> buck2_data::ActionName {
        buck2_data::ActionName {
            category: self.action.category().as_str().to_owned(),
            identifier: self.action.identifier().unwrap_or("").to_owned(),
        }
    }
}
