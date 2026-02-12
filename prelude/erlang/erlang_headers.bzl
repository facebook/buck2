# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load(":erlang_build.bzl", "erlang_build")
load(":erlang_toolchain.bzl", "get_toolchain")

def _erlang_headers(ctx: AnalysisContext) -> list[Provider]:
    toolchain = get_toolchain(ctx)

    output = ctx.actions.declare_output("include")
    wildcard = paths.join("erts-*", "include")
    erlang_build.utils.run_with_env(
        ctx,
        toolchain,
        cmd_args(toolchain.extract_from_otp, wildcard, output.as_output()),
        identifier = ctx.attrs.name,
        category = "extract_otp_header",
    )

    return [
        DefaultInfo(
            default_outputs = [output],
        ),
    ]

erlang_headers = rule(
    impl = _erlang_headers,
    attrs = {
        "_toolchain": attrs.toolchain_dep(default = "toolchains//:erlang-default"),
    },
)
