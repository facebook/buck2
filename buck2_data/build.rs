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
    let proto_files = &["data.proto"];

    buck2_protoc_dev::configure()
        .setup_protoc("../../..")
        .type_attribute(
            "buck.data.BuckEvent.data",
            "#[allow(clippy::large_enum_variant)]",
        )
        .type_attribute(
            "buck.data.SpanEndEvent.data",
            "#[allow(clippy::large_enum_variant)]",
        )
        .type_attribute(
            "buck.data.BuckEvent.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute(
            "buck.data.SpanStartEvent.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute(
            "buck.data.SpanEndEvent.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute(
            "buck.data.CommandStart.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute(
            "buck.data.CommandEnd.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute(
            "buck.data.CommandCriticalStart.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute(
            "buck.data.CommandCriticalEnd.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute(
            "buck.data.InstantEvent.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute(
            "buck.data.RecordEvent.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute(
            "buck.data.LocalStage.stage",
            "#[derive(::derive_more::From)]",
        )
        .type_attribute("buck.data.ReStage.stage", "#[derive(::derive_more::From)]")
        .type_attribute(
            "buck.data.ExecutorStageStart.stage",
            "#[derive(::derive_more::From)]",
        )
        .type_attribute(
            "buck.data.CommandExecution.status",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute(
            "buck.data.ActionExecutionEnd.error",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute(
            "buck.data.CommandExecutionDetails.command",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName)]",
        )
        .type_attribute("buck.data.TargetLabel", "#[derive(Eq, Hash)]")
        .type_attribute("buck.data.Configuration", "#[derive(Eq, Hash)]")
        .type_attribute("buck.data.ConfiguredTargetLabel", "#[derive(Eq, Hash)]")
        .type_attribute("buck.data.BxlFunctionLabel", "#[derive(Eq, Hash)]")
        .type_attribute("buck.data.BxlFunctionKey", "#[derive(Eq, Hash)]")
        .type_attribute("buck.data.ActionKey.owner", "#[derive(Eq, Hash)]")
        .type_attribute("buck.data.ActionKey", "#[derive(Eq, Hash)]")
        .type_attribute(
            "buck.data.ActionKind",
            "#[derive(::gazebo::variants::VariantName)]",
        )
        .type_attribute(".", "#[derive(::serde::Serialize, ::serde::Deserialize)]")
        .type_attribute(".", "#[derive(::allocative::Allocative)]")
        .field_attribute(
            "timestamp",
            "#[serde(with = \"crate::serialize_timestamp\")]",
        )
        .field_attribute("duration", "#[serde(with = \"crate::serialize_duration\")]")
        .field_attribute(
            "command_duration",
            "#[serde(with = \"crate::serialize_duration\")]",
        )
        .field_attribute(
            "client_walltime",
            "#[serde(with = \"crate::serialize_duration\")]",
        )
        .field_attribute(
            "critical_path_duration",
            "#[serde(with = \"crate::serialize_duration\")]",
        )
        .field_attribute(
            "ActionExecutionEnd.wall_time",
            "#[serde(with = \"crate::serialize_duration\")]",
        )
        .field_attribute(
            "ActionKey.id",
            "#[serde(with = \"crate::serialize_bytes\")]",
        )
        // When serializing using Serde we don't want those to just be i32s, since those are
        // meaningless without the Protobuf schema.
        .field_attribute(
            "ActionExecutionStart.kind",
            "#[serde(with = \"crate::serialize_action_kind\")]",
        )
        .field_attribute(
            "ActionExecutionEnd.kind",
            "#[serde(with = \"crate::serialize_action_kind\")]",
        )
        .field_attribute(
            "RemoteCommand.queue_time",
            "#[serde(with = \"crate::serialize_duration\")]",
        )
        .compile(proto_files, &["."])
}
