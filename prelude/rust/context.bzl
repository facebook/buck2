# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//linking:link_info.bzl", "LinkStyle")
load(":build_params.bzl", "CrateType", "Emit")

# Struct for sharing common args between rustc and rustdoc
# (rustdoc just relays bunch of the same args to rustc when trying to gen docs)
CommonArgsInfo = record(
    args = field("cmd_args"),
    subdir = field(str.type),
    tempfile = field(str.type),
    short_cmd = field(str.type),
    is_check = field(bool.type),
    crate_map = field({str.type: "label"}),
)

# Compile info which is reusable between multiple compilation command performed
# by the same rule.
CompileContext = record(
    # Symlink root containing all sources.
    symlinked_srcs = field("artifact"),
    # Linker args to pass the linker wrapper to rustc.
    linker_args = field("cmd_args"),
    # Clippy wrapper (wrapping clippy-driver so it has the same CLI as rustc).
    clippy_wrapper = field("cmd_args"),
    # Memoized common args for reuse.
    common_args = field({(CrateType.type, Emit.type, LinkStyle.type): CommonArgsInfo.type}),
)
