/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;

use crate::api::user_data::UserCycleDetector;
use crate::api::user_data::UserCycleDetectorGuard;
use crate::impls::dice::DiceModern;
use crate::impls::key::DiceKey;

/// User supplied cycle detector
pub(crate) struct UserCycleDetectorData {
    detector: Option<Arc<dyn UserCycleDetector>>,
    user_cycle_detector_guard: Option<(DiceKey, Option<Box<dyn UserCycleDetectorGuard>>)>,
    dice: Arc<DiceModern>,
}

impl UserCycleDetectorData {
    pub(crate) fn new(detector: Option<Arc<dyn UserCycleDetector>>, dice: Arc<DiceModern>) -> Self {
        Self {
            detector,
            user_cycle_detector_guard: None,
            dice,
        }
    }

    pub(crate) fn start_computing_key(&mut self, k: DiceKey) {
        assert!(self.user_cycle_detector_guard.is_none());
        if let Some(detector) = self.detector.as_ref() {
            self.user_cycle_detector_guard = Some((
                k,
                detector.start_computing_key(self.dice.key_index.get(k).as_any()),
            ));
        }
    }

    pub(crate) fn finished_computing_key(self) {
        if let Some((k, _)) = self.user_cycle_detector_guard {
            self.detector
                .as_ref()
                .unwrap()
                .finished_computing_key(self.dice.key_index.get(k).as_any())
        }
    }

    pub(crate) fn subrequest(&self, k: DiceKey) -> UserCycleDetectorData {
        if let Some((_, Some(v))) = &self.user_cycle_detector_guard {
            v.add_edge(self.dice.key_index.get(k).as_any());
        }

        Self {
            detector: self.detector.dupe(),
            user_cycle_detector_guard: None,
            dice: self.dice.dupe(),
        }
    }
}
