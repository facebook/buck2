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
use std::path::PathBuf;

fn main() -> io::Result<()> {
    let proto_files = &["forkserver.proto"];

    let data_include = if let Ok(value) = env::var("BUCK_HACK_DATA_PROTOC_INCLUDE") {
        let path = PathBuf::from(value);
        path.parent().unwrap().to_str().unwrap().to_owned()
    } else {
        "../buck2_data".to_owned()
    };

    buck2_protoc_dev::configure()
        .setup_protoc()
        .type_attribute(
            "buck.forkserver.RequestEvent.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName, ::gazebo::variants::UnpackVariants)]",
        )
        .type_attribute(
            "buck.forkserver.EnvDirective.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName, ::gazebo::variants::UnpackVariants)]",
        )
        .extern_path(".buck.data", "::buck2_data")
        .compile(proto_files, &[".", &data_include])
}
