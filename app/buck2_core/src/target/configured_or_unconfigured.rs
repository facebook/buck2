/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use crate::target::configured_target_label::ConfiguredTargetLabel;
use crate::target::label::label::TargetLabel;

pub trait ConfiguredOrUnconfiguredTargetLabel {
    fn unconfigured_label(&self) -> &TargetLabel;
}

impl ConfiguredOrUnconfiguredTargetLabel for TargetLabel {
    fn unconfigured_label(&self) -> &TargetLabel {
        self
    }
}

impl ConfiguredOrUnconfiguredTargetLabel for ConfiguredTargetLabel {
    fn unconfigured_label(&self) -> &TargetLabel {
        self.unconfigured()
    }
}
