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
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::fs::dynamic_actions_action_key::DynamicActionsActionKey;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use dupe::Dupe;

use crate::dynamic::DynamicLambdaResultsKey;

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
