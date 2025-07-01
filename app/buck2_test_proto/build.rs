/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::env;
use std::io;

fn main() -> io::Result<()> {
    let proto_files = &["test.proto"];

    let includes = if let Ok(path) = env::var("BUCK_PROTO_SRCS") {
        vec![path]
    } else {
        vec![
            ".".to_owned(),
            "../buck2_data".to_owned(),
            "../buck2_host_sharing_proto".to_owned(),
        ]
    };

    let builder = buck2_protoc_dev::configure();
    unsafe { builder.setup_protoc() }
        .type_attribute(
            "buck.test.ExecuteResponse2.response",
            "#[allow(clippy::large_enum_variant)]",
        )
        .extern_path(".buck.data", "::buck2_data")
        .extern_path(".buck.host_sharing", "::buck2_host_sharing_proto")
        .compile(proto_files, &includes)
}
