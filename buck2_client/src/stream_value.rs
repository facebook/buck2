/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::BuckEvent;
use cli_proto::CommandResult;
use serde::Deserialize;
use serde::Serialize;

#[derive(Serialize, Deserialize)]
#[allow(clippy::large_enum_variant)]
pub enum StreamValue {
    Result(CommandResult),
    Event(BuckEvent),
}
