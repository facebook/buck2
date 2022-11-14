load(":defs.bzl", "protoc_distribution")

protoc_distribution(
    name = "distribution",
    version = "21.4",
)

alias(
    name = "protoc",
    actual = ":distribution[protoc]",
    visibility = ["PUBLIC"],
)

alias(
    name = "google_protobuf",
    actual = ":distribution[google_protobuf]",
    visibility = ["PUBLIC"],
)
