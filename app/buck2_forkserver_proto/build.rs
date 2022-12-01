/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;

fn main() -> io::Result<()> {
    let proto_files = &["forkserver.proto"];

    buck2_protoc_dev::configure()
        .setup_protoc("../../../..")
        .type_attribute(
            "buck.forkserver.RequestEvent.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName, ::gazebo::variants::UnpackVariants)]",
        )
        .type_attribute(
            "buck.forkserver.EnvDirective.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName, ::gazebo::variants::UnpackVariants)]",
        )
        .compile(proto_files, &["."])
}
