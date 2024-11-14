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
use dupe::Dupe;
use smallvec::SmallVec;
use static_assertions::assert_eq_size;

use crate::configuration::data::ConfigurationData;
use crate::deferred::base_deferred_key::BaseDeferredKey;
use crate::deferred::dynamic::DynamicLambdaIndex;
use crate::deferred::dynamic::DynamicLambdaResultsKey;
use crate::fs::dynamic_actions_action_key::DynamicActionsActionKey;
use crate::target::configured_target_label::ConfiguredTargetLabel;

/// The base key. We can actually get rid of this and just use 'DeferredKey' if rule analysis is an
/// 'Deferred' itself. This is used to construct the composed 'DeferredKey::Deferred' or
/// 'DeferredKey::Base' type.
#[derive(
    Hash,
    Eq,
    PartialEq,
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Allocative
)]

pub enum DeferredHolderKey {
    Base(BaseDeferredKey),
    DynamicLambda(Arc<DynamicLambdaResultsKey>),
}

assert_eq_size!(DeferredHolderKey, [usize; 3]);

impl DeferredHolderKey {
    pub fn testing_new(target_label: &str) -> DeferredHolderKey {
        let target =
            ConfiguredTargetLabel::testing_parse(target_label, ConfigurationData::testing_new());
        let deferred_key = BaseDeferredKey::TargetLabel(target);
        DeferredHolderKey::Base(deferred_key)
    }

    pub fn owner(&self) -> &BaseDeferredKey {
        match self {
            DeferredHolderKey::Base(base) => base,
            DeferredHolderKey::DynamicLambda(lambda) => lambda.owner(),
        }
    }

    fn dynamic_action_id_stack(&self) -> SmallVec<[DynamicLambdaIndex; 5]> {
        let mut stack = SmallVec::new();
        let mut current = self;
        while let DeferredHolderKey::DynamicLambda(lambda) = current {
            stack.push(lambda.dynamic_actions_index());
            current = lambda.holder_key();
        }
        stack.reverse();
        stack
    }

    pub fn starts_with(&self, other: &DeferredHolderKey) -> bool {
        self.owner() == other.owner()
            && self
                .dynamic_action_id_stack()
                .starts_with(&other.dynamic_action_id_stack())
    }

    /// Create action_key information from the ids, uniquely
    /// identifying this action within this target.
    pub fn action_key(&self) -> Option<DynamicActionsActionKey> {
        // FIXME(ndmitchell): We'd like to have some kind of user supplied name/category here,
        // rather than using the usize ids, so things are a bit more stable and as these strings
        // are likely to come up in error messages users might see (e.g. with paths).
        match self {
            DeferredHolderKey::Base(_) => None,
            DeferredHolderKey::DynamicLambda(lambda) => Some(lambda.action_key()),
        }
    }
}
