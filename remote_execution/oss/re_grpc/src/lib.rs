/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

mod client;
mod digest;
mod error;
mod grpc;
mod metadata;
mod request;
mod response;
mod retry;
pub use client::*;
pub use error::*;
pub use grpc::*;
pub use metadata::*;
pub use request::*;
pub use response::*;
pub use retry::*;

pub fn get_network_stats() -> anyhow::Result<NetworkStatisticsResponse> {
    // TODO: Support this in this client.
    Ok(NetworkStatisticsResponse::default())
}
