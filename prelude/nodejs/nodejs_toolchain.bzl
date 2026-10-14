# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

NodejsToolchainInfo = provider(
    doc = "Node.js toolchain.",
    fields = {
        "node": provider_field(RunInfo),
        # Targets that set `tsconfig` fail when this is `None`.
        "tsc": provider_field([RunInfo, None], default = None),
    },
)

def _nodejs_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs.node == None:
        fail("nodejs_toolchain requires the `node` attribute")
    return [
        DefaultInfo(),
        NodejsToolchainInfo(
            node = ctx.attrs.node[RunInfo],
            tsc = ctx.attrs.tsc[RunInfo] if ctx.attrs.tsc != None else None,
        ),
    ]

nodejs_toolchain = rule(
    impl = _nodejs_toolchain_impl,
    attrs = {
        "node": attrs.option(attrs.dep(providers = [RunInfo]), default = None),
        "tsc": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
    },
    is_toolchain_rule = True,
)

def _system_nodejs_toolchain_impl(_ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        NodejsToolchainInfo(node = RunInfo(args = ["node"])),
    ]

system_nodejs_toolchain = rule(
    impl = _system_nodejs_toolchain_impl,
    attrs = {},
    is_toolchain_rule = True,
)
