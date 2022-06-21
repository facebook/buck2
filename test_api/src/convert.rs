//! Utilities for GRPC conversions

use std::convert::TryInto;
use std::time::Duration;

use anyhow::Context as _;

pub fn to_std_duration(d: prost_types::Duration) -> anyhow::Result<Duration> {
    Ok(Duration::new(
        d.seconds.try_into().context("Invalid `seconds`")?,
        d.nanos.try_into().context("Invalid `nanos`")?,
    ))
}
