/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Utilities for GRPC conversions

use std::time::Duration;

use anyhow::Context as _;

pub fn to_std_duration(d: prost_types::Duration) -> anyhow::Result<Duration> {
    Ok(Duration::new(
        d.seconds.try_into().context("Invalid `seconds`")?,
        d.nanos.try_into().context("Invalid `nanos`")?,
    ))
}
