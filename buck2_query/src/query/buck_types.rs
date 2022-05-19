/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::target::{ConfiguredTargetLabel, TargetLabel};

use crate::query::environment::NodeLabel;

// We implement some query traits for types in buck2_core so they can be used in query environments.

impl NodeLabel for ConfiguredTargetLabel {}
impl NodeLabel for TargetLabel {}
