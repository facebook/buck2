# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @lint-ignore FBCODEBZLADDLOADS

def rust_library(
        rustc_flags = [],
        deps = [],
        named_deps = None,
        os_deps = None,
        test_deps = None,
        test_env = None,
        visibility = ["PUBLIC"],
        **kwargs):
    _unused = (test_deps, test_env, named_deps)  # @unused
    deps = _fix_deps(deps)
    if os_deps:
        deps += _select_os_deps(_fix_dict_deps(os_deps))
    native.rust_library(
        rustc_flags = rustc_flags + [_CFG_BUCK_OSS_BUILD],
        deps = deps,
        visibility = visibility,
        **kwargs
    )

def rust_binary(
        rustc_flags = [],
        deps = [],
        unittests = None,
        visibility = ["PUBLIC"],
        **kwargs):
    _unused = unittests  # @unused
    deps = _fix_deps(deps)
    native.rust_binary(
        rustc_flags = rustc_flags + [_CFG_BUCK_OSS_BUILD],
        deps = deps,
        visibility = visibility,
        **kwargs
    )

def rust_protobuf_library(
        name,
        srcs,
        build_script,
        protos,
        build_env = None,
        deps = None):
    deps = _fix_deps(deps) if deps else None
    if build_env:
        build_env = {
            k: _fix_dep_in_string(v)
            for k, v in build_env.items()
        }

    build_name = name + "-build"
    proto_name = name + "-proto"

    rust_binary(
        name = build_name,
        srcs = [build_script],
        crate_root = build_script,
        deps = [
            "fbsource//third-party/rust:tonic-build",
            "//buck2/app/buck2_protoc_dev:buck2_protoc_dev",
        ],
    )

    build_env = build_env or {}
    build_env.update(
        {
            "PROTOC": "$(exe buck//third-party/proto:protoc)",
            "PROTOC_INCLUDE": "$(location buck//third-party/proto:google_protobuf)",
        },
    )

    native.genrule(
        name = proto_name,
        srcs = protos + [
            "buck//third-party/proto:google_protobuf",
        ],
        out = ".",
        cmd = "$(exe :" + build_name + ")",
        env = build_env,
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
    for proto in protos:
        native.export_file(
            name = proto,
            visibility = ["PUBLIC"],
        )

# Configuration that is used when building open source using Buck2 as the build system.
# E.g. not applied either internally, or when using Cargo to build the open source code.
# At the moment of writing, mostly used to disable jemalloc.
_CFG_BUCK_OSS_BUILD = "--cfg=buck_oss_build"

def _select_os_deps(xss: [(
    "string",
    ["string"],
)]) -> "selector":
    d = {
        "prelude//os:" + os: xs
        for os, xs in xss
    }
    d["DEFAULT"] = []
    return select(d)

def _fix_dict_deps(xss: [(
    "string",
    ["string"],
)]) -> [(
    "string",
    ["string"],
)]:
    return [
        (k, _fix_deps(xs))
        for k, xs in xss
    ]

def _fix_deps(xs: ["string"]) -> ["string"]:
    return filter(None, map(_fix_dep, xs))

def _fix_dep(x: "string") -> [
    None,
    "string",
]:
    if x == "fbsource//third-party/blake3:blake3-rust":
        x = "fbsource//third-party/rust:blake3"

    if x == "//buck2/gazebo/gazebo:gazebo":
        return "fbsource//third-party/rust:gazebo"
    elif x == "//common/rust/folly/logging:logging":
        return None
    elif x == "//watchman/rust/watchman_client:watchman_client":
        return "fbsource//third-party/rust:watchman_client"
    elif x.startswith("//common/rust/shed/"):
        return "fbsource//third-party/rust:" + x.removeprefix("//common/rust/shed/").split(":")[0]
    elif x.startswith("//common/rust/") or x.startswith("//buck2/facebook/") or x.startswith("//eden/") or x.startswith("//remote_execution/"):
        return None
    elif x.startswith("//buck2/"):
        return "root//" + x.removeprefix("//buck2/")
    else:
        return x

def _fix_dep_in_string(x: "string") -> "string":
    """Replace internal labels in string values such as env-vars."""
    return (x
        .replace("//buck2/", "root//"))
