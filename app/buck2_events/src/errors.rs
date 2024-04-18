/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use gazebo::prelude::SliceExt;

pub fn create_error_report(err: &buck2_error::Error) -> buck2_data::ErrorReport {
    // Infra error by default if no category tag is set
    let category = err.get_category().map(|c| match c {
        buck2_error::Tier::Input => buck2_data::error::ErrorTier::Input,
        buck2_error::Tier::Tier0 => buck2_data::error::ErrorTier::Tier0,
    });
    let typ = err.get_error_type().map(|c| c as i32);

    let (message, telemetry_message) = if let Some(f) = err.is_emitted() {
        (format!("{:?}", f), Some(format!("{:?}", err)))
    } else {
        (format!("{:?}", err), None)
    };

    let source_location = err.source_location().map(ToOwned::to_owned);

    buck2_data::ErrorReport {
        tier: category.map(|c| c as i32),
        typ,
        message,
        telemetry_message,
        source_location,
        tags: err.tags().map(|t| *t as i32),
    }
}
