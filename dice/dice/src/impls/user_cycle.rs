/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use crate::api::error::DiceError;
use crate::api::error::DiceErrorImpl;
use crate::api::error::DiceResult;
use crate::api::user_data::UserCycleDetector;
use crate::api::user_data::UserCycleDetectorGuard;
use crate::impls::key::DiceKey;
use crate::impls::key_index::DiceKeyIndex;

/// User supplied cycle detector
pub(crate) struct UserCycleDetectorData {
    user_cycle_detector_guard: Option<(DiceKey, Option<Box<dyn UserCycleDetectorGuard>>)>,
}

impl UserCycleDetectorData {
    pub(crate) fn new() -> Self {
        UserCycleDetectorData {
            user_cycle_detector_guard: None,
        }
    }

    pub(crate) fn start_computing_key(
        &mut self,
        k: DiceKey,
        key_index: &DiceKeyIndex,
        detector: Option<&dyn UserCycleDetector>,
    ) {
        assert!(self.user_cycle_detector_guard.is_none());
        if let Some(detector) = detector {
            self.user_cycle_detector_guard =
                Some((k, detector.start_computing_key(key_index.get(k).as_any())));
        }
    }

    pub(crate) fn finished_computing_key(
        self,
        key_index: &DiceKeyIndex,
        detector: Option<&dyn UserCycleDetector>,
    ) {
        if let Some((k, _)) = self.user_cycle_detector_guard {
            detector
                .unwrap()
                .finished_computing_key(key_index.get(k).as_any())
        }
    }

    pub(crate) fn subrequest(&self, k: DiceKey, key_index: &DiceKeyIndex) -> UserCycleDetectorData {
        if let Some((_, Some(v))) = &self.user_cycle_detector_guard {
            v.add_edge(key_index.get(k).as_any());
        }

        UserCycleDetectorData {
            user_cycle_detector_guard: None,
        }
    }

    pub(crate) fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<&T>> {
        match &self.user_cycle_detector_guard {
            Some((_, Some(guard))) => match guard.as_any().downcast_ref() {
                Some(guard) => Ok(Some(guard)),
                None => Err(DiceError(Arc::new(
                    DiceErrorImpl::UnexpectedCycleGuardType {
                        expected_type_name: std::any::type_name::<T>().to_owned(),
                        actual_type_name: guard.type_name().to_owned(),
                    },
                ))),
            },
            _ => Ok(None),
        }
    }
}
