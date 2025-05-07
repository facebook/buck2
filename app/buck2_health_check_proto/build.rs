/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;
use std::io;

fn main() -> io::Result<()> {
    let proto_files = &["health_check.proto"];

    let includes = if let Ok(path) = env::var("BUCK_PROTO_SRCS") {
        vec![path]
    } else {
        vec![
            ".".to_owned(),
            "../buck2_data".to_owned(),
            "../buck2_host_sharing_proto".to_owned(),
        ]
    };

    buck2_protoc_dev::configure()
        .setup_protoc()
        .extern_path(".buck.data", "::buck2_data")
        .compile(proto_files, &includes)
}
