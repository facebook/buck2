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
    let proto_files = &["subscription.proto"];

    buck2_protoc_dev::configure()
        .setup_protoc()
        .type_attribute(".", "#[derive(::serde::Serialize, ::serde::Deserialize)]")
        .type_attribute(".", "#[derive(::allocative::Allocative)]")
        .type_attribute(
            "buck.subscription.SubscriptionRequest.request",
            "#[derive(::derive_more::From)]",
        )
        .type_attribute(
            "buck.subscription.SubscriptionResponse.response",
            "#[derive(::derive_more::From)]",
        )
        .compile(proto_files, &["."])
}
