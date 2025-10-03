use std::io;

fn main() -> io::Result<()> {
    let proto_files = &[
        "proto/google/api/annotations.proto",
        "proto/google/api/client.proto",
        "proto/google/api/field_behavior.proto",
        "proto/google/api/http.proto",
        "proto/google/bytestream/bytestream.proto",
        "proto/google/longrunning/operations.proto",
        "proto/google/rpc/code.proto",
        "proto/google/rpc/status.proto",
    ];

    let builder = buck2_protoc_dev::configure();
    unsafe { builder.setup_protoc() }
        .type_attribute(".", "#[derive(::serde::Serialize, ::serde::Deserialize)]")
        .field_attribute(
            "google.longrunning.Operation.metadata",
            "#[serde(with = \"crate::serialize_option_any\")]",
        )
        .field_attribute(
            "google.longrunning.Operation.result.response",
            "#[serde(with = \"crate::serialize_any\")]",
        )
        .field_attribute(
            "google.longrunning.WaitOperationRequest.timeout",
            "#[serde(with = \"::buck2_data::serialize_duration_as_micros\")]",
        )
        .field_attribute(
            "google.rpc.Status.details",
            "#[serde(with = \"crate::serialize_vec_any\")]",
        )
        .compile(proto_files, &["./proto/"])
}
