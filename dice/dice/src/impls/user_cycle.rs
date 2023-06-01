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

use crate::api::error::DiceError;
use crate::api::error::DiceErrorImpl;
use crate::api::error::DiceResult;
use crate::api::user_data::UserCycleDetector;
use crate::api::user_data::UserCycleDetectorGuard;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key_index::DiceKeyIndex;

pub(crate) struct UserCycleDetectorData(());

impl UserCycleDetectorData {
    pub(crate) fn start_computing_key(
        self,
        k: DiceKey,
        key_index: &DiceKeyIndex,
        detector: Option<&Arc<dyn UserCycleDetector>>,
    ) -> KeyComputingUserCycleDetectorData {
        if let Some(detector) = detector {
            let k = key_index.get(k);
            if let Some(guard) = detector.start_computing_key(k.as_any()) {
                return KeyComputingUserCycleDetectorData::Detecting {
                    k: k.dupe(),
                    guard,
                    detector: detector.dupe(),
                };
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
pub(crate) enum KeyComputingUserCycleDetectorData {
    Detecting {
        k: DiceKeyErased,
        guard: Box<dyn UserCycleDetectorGuard>,
        detector: Arc<dyn UserCycleDetector>,
    },
    Untracked,
}

impl KeyComputingUserCycleDetectorData {
    pub(crate) fn finished_computing_key(self) {
        match self {
            KeyComputingUserCycleDetectorData::Detecting { k, detector, .. } => {
                detector.finished_computing_key(k.as_any())
            }
            KeyComputingUserCycleDetectorData::Untracked => {}
        }
    }

    pub(crate) fn subrequest(&self, k: DiceKey, key_index: &DiceKeyIndex) -> UserCycleDetectorData {
        match self {
            KeyComputingUserCycleDetectorData::Detecting { guard, .. } => {
                guard.add_edge(key_index.get(k).as_any());
            }
            KeyComputingUserCycleDetectorData::Untracked => {}
        }

        UserCycleDetectorData(())
    }

    pub(crate) fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<&T>> {
        match self {
            KeyComputingUserCycleDetectorData::Detecting { guard, .. } => {
                match guard.as_any().downcast_ref() {
                    Some(guard) => Ok(Some(guard)),
                    None => Err(DiceError(Arc::new(
                        DiceErrorImpl::UnexpectedCycleGuardType {
                            expected_type_name: std::any::type_name::<T>().to_owned(),
                            actual_type_name: guard.type_name().to_owned(),
                        },
                    ))),
                }
            }
            KeyComputingUserCycleDetectorData::Untracked => Ok(None),
        }
    }
}
