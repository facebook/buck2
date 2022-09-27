/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::target::ConfiguredTargetLabel;
use derive_more::Display;
use gazebo::dupe::Dupe;
use gazebo::variants::UnpackVariants;

use crate::bxl::types::BxlKey;

/// Key types for the base 'DeferredKey'
#[derive(
    Clone,
    Dupe,
    Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Ord,
    PartialOrd,
    UnpackVariants
)]
pub enum BaseDeferredKey {
    #[display(fmt = "{}", _0)]
    TargetLabel(ConfiguredTargetLabel),

    #[display(fmt = "{}", _0)]
    BxlLabel(BxlKey),
}
