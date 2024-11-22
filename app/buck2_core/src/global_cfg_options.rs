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

use crate::target::label::label::TargetLabel;

#[derive(
    Default, Debug, Dupe, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Allocative
)]
pub struct GlobalCfgOptions {
    pub target_platform: Option<TargetLabel>,
    pub cli_modifiers: Arc<Vec<String>>,
}
