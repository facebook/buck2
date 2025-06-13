use std::io;

fn main() -> io::Result<()> {
    let proto_files = &[
        "proto/google/devtools/build/v1/publish_build_event.proto",
        "proto/google/devtools/build/v1/build_events.proto",
        "proto/google/devtools/build/v1/build_status.proto",
    ];

    let builder = buck2_protoc_dev::configure();
    unsafe { builder.setup_protoc() }
        .type_attribute(".", "#[derive(::serde::Serialize, ::serde::Deserialize)]")
        .extern_path(".google.api", "::google_api_proto::google::api")
        .extern_path(".google.bytestream", "::google_api_proto::google::bytestream")
        .extern_path(".google.longrunning", "::google_api_proto::google::longrunning")
        .extern_path(".google.rpc", "::google_api_proto::google::rpc")
        .field_attribute(
            "google.devtools.build.v1.BuildEvent.InvocationAttemptStarted.details",
            "#[serde(with = \"google_api_proto::serialize_option_any\")]",
        )
        .field_attribute(
            "google.devtools.build.v1.BuildEvent.InvocationAttemptFinished.details",
            "#[serde(with = \"google_api_proto::serialize_option_any\")]",
        )
        .field_attribute(
            "google.devtools.build.v1.BuildEvent.BuildEnqueued.details",
            "#[serde(with = \"google_api_proto::serialize_option_any\")]",
        )
        .field_attribute(
            "google.devtools.build.v1.BuildEvent.BuildFinished.details",
            "#[serde(with = \"google_api_proto::serialize_option_any\")]",
        )
        .field_attribute(
            "google.devtools.build.v1.BuildEvent.event.bazel_event",
            "#[serde(with = \"google_api_proto::serialize_any\")]",
        )
        .field_attribute(
            "google.devtools.build.v1.BuildEvent.event.build_execution_event",
            "#[serde(with = \"google_api_proto::serialize_any\")]",
        )
        .field_attribute(
            "google.devtools.build.v1.BuildEvent.event.source_fetch_event",
            "#[serde(with = \"google_api_proto::serialize_any\")]",
        )
        .field_attribute(
            "google.devtools.build.v1.BuildEvent.event.build_tool_event",
            "#[serde(with = \"google_api_proto::serialize_any\")]",
        )
        .field_attribute(
            "google.devtools.build.v1.BuildStatus.details",
            "#[serde(with = \"google_api_proto::serialize_option_any\")]",
        )
        .field_attribute(
            "google.devtools.build.v1.PublishLifecycleEventRequest.stream_timeout",
            "#[serde(with = \"::buck2_data::serialize_duration_as_micros\")]",
        )
        .field_attribute(
            "google.devtools.build.v1.BuildEvent.event_time",
            "#[serde(with = \"::buck2_data::serialize_timestamp\")]",
        )
        .compile(
            proto_files,
            &["./proto/", "../google_api_proto/proto"],
        )
}
