/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use serde::Deserialize;

#[derive(Debug, Deserialize, Allocative)]
#[serde(deny_unknown_fields)]
pub(crate) struct BuckSettingsData {}

#[derive(Clone, Dupe, Debug, Allocative)]
pub struct BuckSettings(
    #[expect(dead_code, reason = "read in next diff")] pub(crate) Arc<BuckSettingsData>,
);

impl BuckSettings {
    pub fn empty() -> Self {
        Self(Arc::new(BuckSettingsData {}))
    }
}
