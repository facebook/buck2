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
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_execute::base_deferred_key_dyn::BaseDeferredKeyDyn;
use buck2_execute::bxl::types::BxlKey;
use derive_more::Display;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;

use crate::analysis::anon_target_node::AnonTarget;

/// Key types for the base 'DeferredKey'
#[derive(
    Clone,
    Dupe,
    Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    UnpackVariants,
    Allocative
)]
pub enum BaseDeferredKey {
    TargetLabel(ConfiguredTargetLabel),
    AnonTarget(Arc<AnonTarget>),
    BxlLabel(BxlKey),
}

impl BaseDeferredKey {
    pub fn into_dyn(self) -> BaseDeferredKeyDyn {
        match self {
            BaseDeferredKey::TargetLabel(label) => BaseDeferredKeyDyn::TargetLabel(label),
            BaseDeferredKey::AnonTarget(target) => BaseDeferredKeyDyn::Dyn(target),
            BaseDeferredKey::BxlLabel(label) => {
                BaseDeferredKeyDyn::Dyn(label.into_base_deferred_key_dyn_impl())
            }
        }
    }
}
