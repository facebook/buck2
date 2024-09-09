# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_toolchain_types.bzl", "DistLtoToolsInfo", "LinkerType")

def _impl(ctx):
    return [
        DefaultInfo(),
        DistLtoToolsInfo(
            planner = {
                LinkerType(linker_type): planner[RunInfo]
                for linker_type, planner in ctx.attrs.planner.items()
            },
            opt = {
                LinkerType(linker_type): opt[RunInfo]
                for linker_type, opt in ctx.attrs.opt.items()
            },
            prepare = ctx.attrs.prepare[RunInfo],
            copy = ctx.attrs.copy[RunInfo],
        ),
    ]

dist_lto_tools = rule(
    impl = _impl,
    attrs = {
        "copy": attrs.dep(providers = [RunInfo]),
        "opt": attrs.dict(
            key = attrs.enum(LinkerType.values()),
            value = attrs.dep(providers = [RunInfo]),
        ),
        "planner": attrs.dict(
            key = attrs.enum(LinkerType.values()),
            value = attrs.dep(providers = [RunInfo]),
        ),
        "prepare": attrs.dep(providers = [RunInfo]),
    },
)
