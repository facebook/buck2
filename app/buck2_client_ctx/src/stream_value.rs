/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use cli_proto::CommandResult;
use serde::Deserialize;
use serde::Serialize;

#[derive(Deserialize)]
#[allow(clippy::large_enum_variant)]
pub enum StreamValue {
    Result(CommandResult),
    Event(buck2_data::BuckEvent),
}

impl StreamValue {
    pub fn as_ref(&self) -> StreamValueRef {
        match self {
            StreamValue::Result(result) => StreamValueRef::Result(result),
            StreamValue::Event(event) => StreamValueRef::Event(event),
        }
    }
}

#[derive(Serialize)]
pub enum StreamValueRef<'a> {
    Result(&'a CommandResult),
    Event(&'a buck2_data::BuckEvent),
}
