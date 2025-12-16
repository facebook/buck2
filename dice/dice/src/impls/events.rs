/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use dupe::Dupe;

use crate::api::events::DiceEvent;
use crate::api::events::DiceEventListener;
use crate::impls::dice::Dice;
use crate::impls::key::DiceKey;

#[derive(Clone, Dupe)]
pub(crate) struct DiceEventDispatcher {
    tracker: Arc<dyn DiceEventListener>,
    dice: Arc<Dice>,
}

impl DiceEventDispatcher {
    pub(crate) fn new(tracker: Arc<dyn DiceEventListener>, dice: Arc<Dice>) -> Self {
        Self { tracker, dice }
    }

    pub(crate) fn started(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.tracker.event(DiceEvent::Started { key_type: desc })
    }

    pub(crate) fn finished(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.tracker.event(DiceEvent::Finished { key_type: desc })
    }

    pub(crate) fn check_deps_started(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.tracker
            .event(DiceEvent::CheckDepsStarted { key_type: desc })
    }

    pub(crate) fn check_deps_finished(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.tracker
            .event(DiceEvent::CheckDepsFinished { key_type: desc })
    }

    pub(crate) fn compute_started(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.tracker
            .event(DiceEvent::ComputeStarted { key_type: desc })
    }

    pub(crate) fn compute_finished(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.tracker
            .event(DiceEvent::ComputeFinished { key_type: desc })
    }
}
