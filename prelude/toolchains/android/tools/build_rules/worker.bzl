# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//toolchains/android/tools/build_rules:java_rules.bzl", "OPEN_JDK_COMPILER_ARGS")

def _jvm_arg_name_is_specified(arg_name: str, existing_jvm_args: list[str]) -> bool:
    for existing_jvm_arg in existing_jvm_args:
        if existing_jvm_arg.startswith(arg_name):
            return True

    return False

def _worker_impl(ctx):
    args = cmd_args()
    args.add(ctx.attrs.java[RunInfo])
    jvm_args = ctx.attrs.jvm_args
    if not "-XX:-MaxFDLimit" in jvm_args:
        jvm_args.insert(0, "-XX:-MaxFDLimit")
    if not _jvm_arg_name_is_specified("-Dfile.encoding", jvm_args):
        jvm_args.insert(0, "-Dfile.encoding=UTF-8")
    if not _jvm_arg_name_is_specified("-XX:CompressedClassSpaceSize", jvm_args):
        jvm_args.insert(0, "-XX:CompressedClassSpaceSize=2g")

    # Allow JVM compiler daemon to access internal jdk.compiler APIs
    jvm_args.extend(OPEN_JDK_COMPILER_ARGS)
    jvm_args.append("--add-opens=java.base/java.util=ALL-UNNAMED")
    args.add(jvm_args)
    args.add([
        "-cp",
        ctx.attrs.exe,
        "-jar",
        ctx.attrs.class_loader_bootstrapper,
        ctx.attrs.main_class,
    ])

    return [
        DefaultInfo(),
        WorkerInfo(
            exe = RunInfo(args),
        ),
    ]

worker = rule(
    impl = _worker_impl,
    attrs = {
        "class_loader_bootstrapper": attrs.source(default = "prelude//toolchains/android/src/com/facebook/buck/cli/bootstrapper:bootstrapper"),
        "concurrency": attrs.option(attrs.int(), default = None),
        "exe": attrs.source(),
        "java": attrs.default_only(attrs.dep(default = "fbsource//third-party/toolchains/jdk:java")),
        "jvm_args": attrs.list(attrs.string(), default = []),
        "main_class": attrs.string(),
    },
)
