/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_events::dispatch::EventDispatcher;
use dice::DiceComputations;
use dice::UserComputationData;

pub trait HasEvents {
    fn get_dispatcher(&self) -> &EventDispatcher;
}

impl HasEvents for UserComputationData {
    fn get_dispatcher(&self) -> &EventDispatcher {
        self.data
            .get::<EventDispatcher>()
            .expect("Event dispatcher should be set")
    }
}

impl HasEvents for DiceComputations<'_> {
    fn get_dispatcher(&self) -> &EventDispatcher {
        self.per_transaction_data().get_dispatcher()
    }
}
