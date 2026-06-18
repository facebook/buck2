/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::any::Any;

use allocative::Allocative;
use buck2_artifact::actions::key::ActionKey;
use buck2_error::TypedContext;
use buck2_hash::BuckDashSet;
use dice::UserComputationData;
use dupe::Dupe;

#[derive(Allocative, Clone, Debug, Eq, PartialEq)]
pub struct ActionRewindRequest {
    action_keys: Vec<ActionKey>,
}

impl ActionRewindRequest {
    pub fn new(action_keys: Vec<ActionKey>) -> Self {
        Self { action_keys }
    }

    pub fn action_keys(&self) -> &[ActionKey] {
        &self.action_keys
    }

    pub fn into_action_keys(self) -> Vec<ActionKey> {
        self.action_keys
    }

    pub fn merge(&mut self, other: &Self) {
        for key in &other.action_keys {
            if !self.action_keys.iter().any(|existing| existing == key) {
                self.action_keys.push(key.dupe());
            }
        }
    }
}

impl TypedContext for ActionRewindRequest {
    fn eq(&self, other: &dyn TypedContext) -> bool {
        (other as &dyn Any).downcast_ref::<Self>() == Some(self)
    }

    fn display(&self) -> Option<String> {
        Some(format!("ActionRewindRequest({:?})", self.action_keys))
    }
}

pub struct ActionRewindTrackerHolder(BuckDashSet<ActionKey>);

pub trait HasActionRewindTracker {
    fn init_action_rewind_tracker(&mut self);

    fn set_rewound_actions(&self, action_keys: &[ActionKey]);

    fn is_action_rewound(&self, action_key: &ActionKey) -> bool;
}

impl HasActionRewindTracker for UserComputationData {
    fn init_action_rewind_tracker(&mut self) {
        self.data
            .set(ActionRewindTrackerHolder(BuckDashSet::default()));
    }

    fn set_rewound_actions(&self, action_keys: &[ActionKey]) {
        let holder = self
            .data
            .get::<ActionRewindTrackerHolder>()
            .expect("ActionRewindTracker should be set");
        for action_key in action_keys {
            holder.0.insert(action_key.dupe());
        }
    }

    fn is_action_rewound(&self, action_key: &ActionKey) -> bool {
        self.data
            .get::<ActionRewindTrackerHolder>()
            .is_ok_and(|holder| holder.0.contains(action_key))
    }
}

#[cfg(test)]
mod tests {
    use buck2_artifact::actions::key::ActionIndex;
    use buck2_core::deferred::key::DeferredHolderKey;
    use dice::UserComputationData;

    use super::*;

    fn action_key(id: u32) -> ActionKey {
        ActionKey::new(
            DeferredHolderKey::testing_new("cell//pkg:target"),
            ActionIndex::new(id),
        )
    }

    #[test]
    fn rewound_actions_accumulate_across_rewinds() {
        let mut data = UserComputationData::new();
        data.init_action_rewind_tracker();

        let first = action_key(0);
        let second = action_key(1);

        data.set_rewound_actions(&[first.clone()]);
        data.set_rewound_actions(&[second.clone()]);

        assert!(data.is_action_rewound(&first));
        assert!(data.is_action_rewound(&second));
    }
}
