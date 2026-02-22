# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@fbcode//buck2:buck_rust_binary.bzl", "buck_rust_binary")
load("@fbcode_macros//build_defs:native_rules.bzl", "alias", "buck_genrule")
load("@fbsource//tools/build_defs:rust_library.bzl", "rust_library")

def rust_protobuf_library(
        name,
        srcs,
        build_script,
        protos = None,  # Pass a list of files. Thye'll be placed in the cwd. Prefer using proto_srcs.
        deps = None,
        test_deps = None,
        doctests = True,
        build_env = None,
        proto_srcs = None):  # Use a proto_srcs() target, path is exposed as BUCK_PROTO_SRCS.
    _rust_protobuf_library(
        name,
        srcs,
        build_script,
        "buck2_protoc_dev",
        "0.12",
        protos,
        [
            "fbsource//third-party/rust:tonic-0-10",
        ] + (deps or []),
        test_deps,
        doctests,
        build_env,
        proto_srcs,
        None,
    )

    # Set up an alias to the default version of prost to avoid breaking callers
    alias(
        name = name,
        actual = ":" + name + "_prost",
    )

def rust_protobuf_library_prost_0134(
        name,
        srcs,
        build_script,
        protos = None,  # Pass a list of files. Thye'll be placed in the cwd. Prefer using proto_srcs.
        deps = None,
        test_deps = None,
        doctests = True,
        build_env = None,
        proto_srcs = None,  # Use a proto_srcs() target, path is exposed as BUCK_PROTO_SRCS.
        crate_name = None):
    _rust_protobuf_library(
        name,
        srcs,
        build_script,
        "buck2_protoc_dev-tonic-0-12-3",
        "0.13",
        protos,
        [
            "fbsource//third-party/rust:tonic-0-12",
        ] + (deps or []),
        test_deps,
        doctests,
        build_env,
        proto_srcs,
        crate_name,
    )

    # Set up an alias to the default version of prost to avoid breaking callers
    alias(
        name = name,
        actual = ":" + name + "_prost-0-13-4",
    )

def _rust_protobuf_library(
        name,
        srcs,
        build_script,
        buck2_protoc_dev,
        prost_version,
        protos,
        deps,
        test_deps,
        doctests,
        build_env,
        proto_srcs,
        crate_name):
    versioned_prost_target = {
        "0.12": "prost",
        "0.13": "prost-0-13-4",
    }[prost_version]
    build_name = name + "-build" + "-" + versioned_prost_target
    proto_name = name + "-proto" + "-" + versioned_prost_target

    buck_rust_binary(
        name = build_name,
        srcs = [build_script],
        crate_root = build_script,
        deps = [
            "fbcode//buck2/app/buck2_protoc_dev:" + buck2_protoc_dev,
        ],
    )

    build_env = build_env or {}
    build_env.update({
        "PROTOC": "$(exe fbsource//third-party/protobuf:protoc)",
        "PROTOC_INCLUDE": "$(location fbsource//third-party/protobuf:google.protobuf)",
    })
    if proto_srcs:
        build_env["BUCK_PROTO_SRCS"] = "$(location {})".format(proto_srcs)

    buck_genrule(
        name = proto_name,
        srcs = protos,
        # The binary doesn't look at the command line, but with Buck1, if we don't have $OUT
        # on the command line, it doesn't set the environment variable, so put it on.
        cmd = "$(exe :{}) --required-for-buck1=$OUT".format(build_name),
        env = build_env,
        out = ".",
    )

    new_deps = [{
        "0.12": "fbsource//third-party/rust:prost-0-12",
        "0.13": "fbsource//third-party/rust:prost-0-13-4",
    }[prost_version]] + (deps or [])

    rust_library(
        name = name + "_" + versioned_prost_target,
        crate = crate_name or name,
        srcs = srcs,
        doctests = doctests,
        env = {
            # This is where prost looks for generated .rs files
            "OUT_DIR": "$(location :{})".format(proto_name),
        },
        named_deps = {
            # "prost" is https://github.com/tokio-rs/prost, which is used
            # to generate Rust code from protobuf definitions.
            "generated_prost_target": ":{}".format(proto_name),
        },
        labels = [
            "generated_protobuf_library_rust",
        ],
        deps = new_deps,
        test_deps = test_deps,
    )

ProtoSrcsInfo = provider(fields = ["srcs"])

def _proto_srcs_impl(ctx):
    srcs = {src.basename: src for src in ctx.attrs.srcs}
    for dep in ctx.attrs.deps:
        for src in dep[ProtoSrcsInfo].srcs:
            if src.basename in srcs:
                fail("Duplicate src:", src.basename)
            srcs[src.basename] = src
    out = ctx.actions.copied_dir(ctx.attrs.name, srcs)
    return [DefaultInfo(default_output = out), ProtoSrcsInfo(srcs = srcs.values())]

proto_srcs = rule(
    impl = _proto_srcs_impl,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
        "srcs": attrs.list(attrs.source(), default = []),
    },
)
