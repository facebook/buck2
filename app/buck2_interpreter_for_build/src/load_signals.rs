/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use buck2_core::package::PackageLabel;
use buck2_node::nodes::eval_result::EvaluationResult;
use dice::UserComputationData;

pub trait SetLoadSignals {
    fn set_load_signals(&mut self, sender: impl LoadSignalSender);
}

impl SetLoadSignals for UserComputationData {
    fn set_load_signals(&mut self, sender: impl LoadSignalSender) {
        self.data.set(Box::new(sender) as Box<dyn LoadSignalSender>);
    }
}

pub trait HasLoadSignals {
    fn get_load_signals(&self) -> Option<&dyn LoadSignalSender>;
}

impl HasLoadSignals for UserComputationData {
    fn get_load_signals(&self) -> Option<&dyn LoadSignalSender> {
        match self.data.get::<Box<dyn LoadSignalSender>>() {
            Ok(s) => Some(&**s),
            Err(..) => None,
        }
    }
}

pub trait LoadSignalSender: Send + Sync + 'static {
    fn send_load(&self, package: PackageLabel, res: &EvaluationResult, duration: Duration);
}
