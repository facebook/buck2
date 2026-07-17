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

struct SettingKey<T> {
    internal_default: T,
    oss_default: T,
}

impl<T: Clone> SettingKey<T> {
    fn default_value(&self) -> T {
        if cfg!(fbcode_build) {
            self.internal_default.clone()
        } else {
            self.oss_default.clone()
        }
    }

    fn resolve(&self, value: Option<T>) -> T {
        value.unwrap_or_else(|| self.default_value())
    }
}

const LOG_USE_MANIFOLD: SettingKey<bool> = SettingKey {
    internal_default: true,
    oss_default: false,
};

#[derive(Debug, Default, Deserialize, Allocative)]
#[serde(deny_unknown_fields)]
pub(crate) struct BuckSettingsData {
    log_use_manifold: Option<bool>,
}

#[derive(Clone, Dupe, Debug, Allocative)]
pub struct BuckSettings(pub(crate) Arc<BuckSettingsData>);

impl BuckSettings {
    pub fn empty() -> Self {
        Self(Arc::new(BuckSettingsData::default()))
    }

    pub fn log_use_manifold(&self) -> bool {
        LOG_USE_MANIFOLD.resolve(self.0.log_use_manifold)
    }
}
