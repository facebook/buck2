# Copyright 2004-present Facebook. All Rights Reserved.
load("@fbcode//buck2:buck_rust_binary.bzl", "buck_rust_binary")
load("@fbcode_macros//build_defs:custom_rule.bzl", "custom_rule")
load("@fbcode_macros//build_defs:export_files.bzl", "export_file")
load("@fbcode_macros//build_defs:rust_library.bzl", "rust_library")

def rust_protobuf_library(name, srcs, build_script, spec, build_env = None, deps = None):
    build_name = name + "-build"
    proto_name = name + "-proto"

    buck_rust_binary(
        name = build_name,
        srcs = [build_script],
        crate_root = build_script,
        deps = [
            "fbsource//third-party/rust:tonic-build",
        ],
    )

    build_env = build_env or {}
    build_env.update(
        {
            "OUT_DIR": "$OUT",
            "PROTOC": "$(exe fbsource//third-party/protobuf:protoc)",
            "PROTOC_INCLUDE": "$(location fbsource//third-party/protobuf:google.protobuf)",
        },
    )

    custom_rule(
        name = proto_name,
        srcs = [
            spec,
        ],
        build_script_dep = ":" + build_name,
        env = build_env,
        output_gen_files = ["."],
    )

    rust_library(
        name = name,
        srcs = srcs,
        env = {
            # This is where tonic looks for generated .rs files
            "OUT_DIR": "$(location :{})".format(proto_name),
        },
        deps = [
            "fbsource//third-party/rust:prost",
            "fbsource//third-party/rust:prost-types",
            "fbsource//third-party/rust:tonic",
        ] + (deps or []),
    )

    # For python tests only
    export_file(
        name = spec,
    )
