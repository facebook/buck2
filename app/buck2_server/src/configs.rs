/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::legacy_configs::diffs::ConfigDiffMetrics;

pub(crate) fn buck_configs(metrics: Option<ConfigDiffMetrics>) -> Vec<buck2_data::CellConfigDiff> {
    let Some(metrics) = metrics else {
        return Vec::new();
    };

    metrics.0
}
