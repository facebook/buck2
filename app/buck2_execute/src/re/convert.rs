/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use remote_execution as RE;

pub fn platform_to_proto(platform: &RE::Platform) -> buck2_data::RePlatform {
    buck2_data::RePlatform {
        properties: platform
            .properties
            .iter()
            .map(|property| buck2_data::re_platform::Property {
                name: property.name.clone(),
                value: property.value.clone(),
            })
            .collect(),
    }
}
