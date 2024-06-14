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
        "proto/google/api/annotations.proto",
        "proto/google/api/client.proto",
        "proto/google/api/http.proto",
        "proto/google/bytestream/bytestream.proto",
        "proto/google/longrunning/operations.proto",
        "proto/google/rpc/code.proto",
        "proto/google/rpc/status.proto",
    ];

    buck2_protoc_dev::configure()
        .setup_protoc()
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
            "google.longrunning.WaitOperationRequest.timeout",
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
            "#[serde(with = \"crate::serialize_vec_any\")]",
        )
        .field_attribute(
            "google.longrunning.Operation.metadata",
            "#[serde(with = \"crate::serialize_option_any\")]",
        )
        .field_attribute(
            "google.longrunning.Operation.result.response",
            "#[serde(with = \"crate::serialize_any\")]",
        )
        .field_attribute(
            "google.rpc.Status.details",
            "#[serde(with = \"crate::serialize_vec_any\")]",
        )
        .compile(proto_files, &["./proto/"])
}
