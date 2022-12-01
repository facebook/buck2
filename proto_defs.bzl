# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode//buck2:buck_rust_binary.bzl", "buck_rust_binary")
load("@fbcode_macros//build_defs:export_files.bzl", "export_file")
load("@fbcode_macros//build_defs:native_rules.bzl", "buck_genrule")
load("@fbcode_macros//build_defs:rust_library.bzl", "rust_library")

def rust_protobuf_library(name, srcs, build_script, protos, build_env = None, deps = None):
    build_name = name + "-build"
    proto_name = name + "-proto"

    buck_rust_binary(
        name = build_name,
        srcs = [build_script],
        crate_root = build_script,
        deps = [
            "//buck2/app/buck2_protoc_dev:buck2_protoc_dev",
        ],
    )

    build_env = build_env or {}
    build_env.update(
        {
            "PROTOC": "$(exe fbsource//third-party/protobuf:protoc)",
            "PROTOC_INCLUDE": "$(location fbsource//third-party/protobuf:google.protobuf)",
        },
    )

    buck_genrule(
        name = proto_name,
        srcs = protos,
        # The binary doesn't look at the command line, but with Buck1, if we don't have $OUT
        # on the command line, it doesn't set the environment variable, so put it on.
        cmd = "$(exe :{}) --required-for-buck1=$OUT".format(build_name),
        env = build_env,
        out = ".",
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
            "fbsource//third-party/rust:tonic",
        ] + (deps or []),
    )

    # For python tests only
    for proto in protos:
        export_file(
            name = proto,
        )
