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
    let proto_files = &["daemon.proto"];

    let data_include = if let Ok(value) = env::var("BUCK_HACK_DATA_PROTOC_INCLUDE") {
        let path = PathBuf::from(value);
        path.parent().unwrap().to_str().unwrap().to_owned()
    } else {
        "../../buck2_data".to_owned()
    };

    let subscription_include = if let Ok(value) = env::var("BUCK_HACK_SUBSCRIPTION_PROTOC_INCLUDE")
    {
        let path = PathBuf::from(value);
        path.parent().unwrap().to_str().unwrap().to_owned()
    } else {
        "../../buck2_subscription_proto".to_owned()
    };

    buck2_protoc_dev::configure()
        .setup_protoc()
        .type_attribute(".", "#[derive(::serde::Serialize, ::serde::Deserialize)] #[serde(rename_all = \"snake_case\")]")
        .type_attribute(".", "#[derive(::allocative::Allocative)]")
        .field_attribute("start_time", "#[serde(with = \"serialize_timestamp\")]")
        .field_attribute("timeout", "#[serde(rename = \"timeout_us\", with = \"buck2_data::serialize_duration_as_micros\")]")
        .field_attribute("uptime", "#[serde(rename = \"uptime_us\", with = \"buck2_data::serialize_duration_as_micros\")]")
        .field_attribute("delay", "#[serde(rename = \"delay_us\", with = \"buck2_data::serialize_duration_as_micros\")]")
        .field_attribute("ProfileResponse.elapsed", "#[serde(rename = \"elapsed_us\", with = \"buck2_data::serialize_duration_as_micros\")]")
        .boxed("CommandProgress.progress.event")
        .boxed("CommandProgress.progress.result")
        .boxed("CommandProgress.progress.partial_result")
        .extern_path(".buck.data", "::buck2_data")
        .extern_path(".buck.subscription", "::buck2_subscription_proto")
        .compile(proto_files, &[".", &data_include, &subscription_include])
}
