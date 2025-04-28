/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::QueryOutputFormat;

#[derive(Debug, Clone)]
pub(crate) enum QueryOutputFormatInfo {
    Default,
    Json,
    Dot,
    DotCompact,
    Starlark,
    Html(String),
}

impl QueryOutputFormatInfo {
    pub fn from_protobuf_int(value: i32, trace_id: String) -> Option<Self> {
        let value = QueryOutputFormat::try_from(value).ok()?;
        let res = match value {
            QueryOutputFormat::Default => Self::Default,
            QueryOutputFormat::Json => Self::Json,
            QueryOutputFormat::Dot => Self::Dot,
            QueryOutputFormat::DotCompact => Self::DotCompact,
            QueryOutputFormat::Starlark => Self::Starlark,
            QueryOutputFormat::Html => Self::Html(trace_id),
        };
        Some(res)
    }
}
