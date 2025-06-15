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
    let proto_files = &[
        "proto/build/bazel/remote/execution/v2/remote_execution.proto",
        "proto/build/bazel/semver/semver.proto",
    ];

    let builder = buck2_protoc_dev::configure();
    unsafe { builder.setup_protoc() }
        .extern_path(".google.api", "::google_api_proto::google::api")
        .extern_path(".google.bytestream", "::google_api_proto::google::bytestream")
        .extern_path(".google.longrunning", "::google_api_proto::google::longrunning")
        .extern_path(".google.rpc", "::google_api_proto::google::rpc")
        .type_attribute(".", "#[derive(::serde::Serialize, ::serde::Deserialize)]")
        .field_attribute(
            "build.bazel.remote.execution.v2.Action.timeout",
            "#[serde(with = \"::buck2_data::serialize_duration_as_micros\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.ExecutedActionMetadata.virtual_execution_duration",
            "#[serde(with = \"::buck2_data::serialize_duration_as_micros\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.NodeProperties.mtime",
            "#[serde(with = \"::buck2_data::serialize_timestamp\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.ExecutedActionMetadata.queued_timestamp",
            "#[serde(with = \"::buck2_data::serialize_timestamp\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.ExecutedActionMetadata.worker_start_timestamp",
            "#[serde(with = \"::buck2_data::serialize_timestamp\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.ExecutedActionMetadata.worker_completed_timestamp",
            "#[serde(with = \"::buck2_data::serialize_timestamp\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.ExecutedActionMetadata.input_fetch_start_timestamp",
            "#[serde(with = \"::buck2_data::serialize_timestamp\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.ExecutedActionMetadata.input_fetch_completed_timestamp",
            "#[serde(with = \"::buck2_data::serialize_timestamp\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.ExecutedActionMetadata.execution_start_timestamp",
            "#[serde(with = \"::buck2_data::serialize_timestamp\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.ExecutedActionMetadata.execution_completed_timestamp",
            "#[serde(with = \"::buck2_data::serialize_timestamp\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.ExecutedActionMetadata.output_upload_start_timestamp",
            "#[serde(with = \"::buck2_data::serialize_timestamp\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.ExecutedActionMetadata.output_upload_completed_timestamp",
            "#[serde(with = \"::buck2_data::serialize_timestamp\")]",
        )
        .field_attribute(
            "build.bazel.remote.execution.v2.ExecutedActionMetadata.auxiliary_metadata",
            "#[serde(with = \"google_api_proto::serialize_vec_any\")]",
        )
        .compile(proto_files, &["./proto/", "../google_api_proto/proto"])
}
