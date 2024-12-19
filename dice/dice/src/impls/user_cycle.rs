/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use dice_error::DiceError;
use dice_error::DiceErrorImpl;
use dice_error::DiceResult;
use dupe::Dupe;

use crate::api::user_data::UserCycleDetector;
use crate::api::user_data::UserCycleDetectorGuard;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key_index::DiceKeyIndex;
use crate::DynKey;

pub(crate) struct UserCycleDetectorData(());

impl UserCycleDetectorData {
    pub(crate) fn start_computing_key(
        self,
        k: DiceKey,
        key_index: &DiceKeyIndex,
        detector: Option<&Arc<dyn UserCycleDetector>>,
    ) -> KeyComputingUserCycleDetectorData {
        if let Some(detector) = detector {
            let k_erased = key_index.get(k);
            if let Some(guard) = detector.start_computing_key(DynKey::ref_cast(k_erased)) {
                debug!("cycles start key {:?}", k);
                return KeyComputingUserCycleDetectorData::Detecting(Arc::new(DetectingData {
                    k_erased: k_erased.dupe(),
                    k,
                    guard,
                    detector: detector.dupe(),
                }));
            }
        }
        KeyComputingUserCycleDetectorData::Untracked
    }

    #[cfg(test)]
    pub(crate) fn testing_new() -> Self {
        Self(())
    }
}

/// User supplied cycle detector
#[derive(Clone)]
pub(crate) enum KeyComputingUserCycleDetectorData {
    Detecting(Arc<DetectingData>),
    Untracked,
}

pub(crate) struct DetectingData {
    k_erased: DiceKeyErased,
    k: DiceKey,
    guard: Arc<dyn UserCycleDetectorGuard>,
    detector: Arc<dyn UserCycleDetector>,
}

impl KeyComputingUserCycleDetectorData {
    pub(crate) fn subrequest(&self, k: DiceKey, key_index: &DiceKeyIndex) -> UserCycleDetectorData {
        match self {
            KeyComputingUserCycleDetectorData::Detecting(data) => {
                data.guard.add_edge(DynKey::ref_cast(key_index.get(k)));
            }
            KeyComputingUserCycleDetectorData::Untracked => {}
        }

        UserCycleDetectorData(())
    }

    pub(crate) fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<Arc<T>>> {
        match self {
            KeyComputingUserCycleDetectorData::Detecting(data) => {
                match data.guard.dupe().as_any_arc().downcast::<T>() {
                    Ok(guard) => Ok(Some(guard)),
                    Err(_) => Err(DiceError(Arc::new(
                        DiceErrorImpl::UnexpectedCycleGuardType {
                            expected_type_name: std::any::type_name::<T>().to_owned(),
                            actual_type_name: data.guard.type_name().to_owned(),
                        },
                    ))),
                }
            }
            KeyComputingUserCycleDetectorData::Untracked => Ok(None),
        }
    }
}

impl Drop for DetectingData {
    fn drop(&mut self) {
        debug!("cycles finish key {:?}", self.k);
        self.detector
            .finished_computing_key(DynKey::ref_cast(&self.k_erased))
    }
}
