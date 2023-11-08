/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_event_observer::display::display_action_error;
use buck2_event_observer::display::TargetDisplayOptions;

#[derive(Debug, Clone)]
pub struct ActionError {
    pub event: buck2_data::ActionError,
    pub owner: BaseDeferredKey,
}

impl std::error::Error for ActionError {}

impl fmt::Display for ActionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = display_action_error(&self.event, TargetDisplayOptions::for_log())
            .expect("Action key is always present in `ActionError`")
            .simple_format_for_build_report();
        write!(f, "{}", s)
    }
}

pub fn late_format_action_error(e: &ActionError, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Failed to build '{}'", e.owner)
}
