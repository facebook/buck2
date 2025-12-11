/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Utilities for GRPC conversions

use std::time::Duration;

pub fn to_std_duration(d: prost_types::Duration) -> buck2_error::Result<Duration> {
    Ok(Duration::new(d.seconds.try_into()?, d.nanos.try_into()?))
}
