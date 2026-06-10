# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":erlang_toolchain.bzl", "get_toolchain")

def _erlang_headers(ctx: AnalysisContext) -> list[Provider]:
    headers = get_toolchain(ctx).erts_toolchain_info.headers
    return [
        DefaultInfo(
            default_outputs = [headers],
        ),
    ]

erlang_headers = rule(
    impl = _erlang_headers,
    attrs = {
        "_toolchain": attrs.toolchain_dep(default = "toolchains//:erlang-default"),
    },
)
