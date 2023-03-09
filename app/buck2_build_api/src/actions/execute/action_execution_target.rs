/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Write;

use buck2_core::category::Category;
use buck2_data::ToProtoMessage;
use buck2_execute::execute::target::CommandExecutionTargetImpl;
use buck2_execute::path::buck_out_path::BuckOutScratchPath;
use derivative::Derivative;
use dupe::Dupe;

use crate::actions::RegisteredAction;
use crate::deferred::base_deferred_key::BaseDeferredKey;

/// Indicates why we are executing a given command.
#[derive(Clone, Dupe, Derivative)]
#[derivative(Debug)]
pub struct ActionExecutionTarget<'a> {
    action: &'a RegisteredAction,
}

impl<'a> ActionExecutionTarget<'a> {
    pub(crate) fn new(action: &'a RegisteredAction) -> Self {
        Self { action }
    }

    pub(crate) fn owner(&self) -> &'a BaseDeferredKey {
        self.action.owner()
    }

    pub(crate) fn category(&self) -> &'a Category {
        self.action.category()
    }

    pub(crate) fn identifier(&self) -> Option<&'a str> {
        self.action.identifier()
    }

    pub(crate) fn custom_tmpdir(&self) -> BuckOutScratchPath {
        BuckOutScratchPath::new(
            self.action.owner().dupe().into_dyn(),
            self.action.category(),
            self.action.identifier(),
        )
        .unwrap()
    }
}

impl CommandExecutionTargetImpl for ActionExecutionTarget<'_> {
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
