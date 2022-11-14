/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_data::ToProtoMessage;
use buck2_execute::base_deferred_key::BaseDeferredKey;
use gazebo::dupe::Dupe;

use crate::actions::RegisteredAction;
use crate::deferred::types::DeferredData;
use crate::deferred::types::DeferredKey;

/// A key to look up an 'Action' from the 'ActionAnalysisResult'.
/// Since 'Action's are registered as 'Deferred's
#[derive(
    Debug,
    Eq,
    PartialEq,
    Hash,
    Clone,
    Dupe,
    derive_more::Display,
    Allocative
)]
pub struct ActionKey(DeferredData<Arc<RegisteredAction>>);

impl ActionKey {
    pub fn new(key: DeferredData<Arc<RegisteredAction>>) -> ActionKey {
        ActionKey(key)
    }

    #[cfg(test)]
    pub fn testing_new(key: DeferredKey) -> ActionKey {
        use crate::deferred::types::testing::DeferredDataExt;
        ActionKey(DeferredData::testing_new(key))
    }

    pub fn deferred_key(&self) -> &DeferredKey {
        self.0.deferred_key()
    }

    pub fn deferred_data(&self) -> &DeferredData<Arc<RegisteredAction>> {
        &self.0
    }

    pub fn owner(&self) -> &BaseDeferredKey {
        self.deferred_key().owner()
    }
}

impl ToProtoMessage for ActionKey {
    type Message = buck2_data::ActionKey;

    fn as_proto(&self) -> Self::Message {
        buck2_data::ActionKey {
            id: self.deferred_key().id().as_usize().to_ne_bytes().to_vec(),
            owner: Some(match self.deferred_key().owner() {
                BaseDeferredKey::TargetLabel(t) => {
                    buck2_data::action_key::Owner::TargetLabel(t.as_proto())
                }
                BaseDeferredKey::BxlLabel(l) => buck2_data::action_key::Owner::BxlKey(l.as_proto()),
            }),
            key: self.deferred_key().action_key(),
        }
    }
}
