# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:native.bzl", "native")
load("@prelude//toolchains/android/tools/build_rules:java_rules.bzl", "buck_java_library")

def protobuf_src_gen(name, srcs, proto_path = [], deps = [], exported_deps = []):
    command = [
        "$(exe prelude//toolchains/android/tools/protobuf:protoc) --java_opt=annotate_code",
        "--proto_path=$SRCDIR",
    ] + ["--proto_path={}".format(path) for path in proto_path] + [
        "--java_out=$TMP/ --grpc-java_out=$TMP/ $SRCS",
        "&& find $TMP -type f -name *.pb.meta -exec rm {} \\;",
        "&& pushd $TMP",
        "&& zip -r {}_gen.src.zip .".format(name),
        "&& popd",
        "&& mv $TMP/{}_gen.src.zip $OUT".format(name),
    ]

    genrule_name = name + "-src-gen"
    native.genrule(
        name = genrule_name,
        srcs = srcs,
        out = name + ".src.zip",
        cmd = " ".join(command),
    )
    buck_java_library(
        name = name,
        srcs = [":" + genrule_name],
        visibility = ["PUBLIC"],
        deps = deps,
        exported_deps = exported_deps,
    )
