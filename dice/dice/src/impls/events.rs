/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use triomphe::Arc;

use crate::api::events::DiceEvent;
use crate::api::events::DiceEventListener;
use crate::impls::dice::DiceModern;
use crate::impls::key::DiceKey;
use crate::impls::key_index::DiceKeyIndex;

pub(crate) struct DiceEventDispatcher {
    tracker: Arc<dyn DiceEventListener>,
    dice: Arc<DiceModern>,
}

impl DiceEventDispatcher {
    pub(crate) fn started(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.tracker.event(DiceEvent::Started { key_type: desc })
    }

    pub(crate) fn finished(&self, k: DiceKey) {
        let desc = self.dice.key_index.get(k).key_type_name();

        self.tracker.event(DiceEvent::Finished { key_type: desc })
    }
}
