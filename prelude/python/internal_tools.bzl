# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# This is a provider that acts much like `PythonToolchainInfo` and is used sort of similarly.
# However, `PythonToolchainInfo` is used for things that are intended to actually be customizable
# on a per-toolchain basis. This is for things that users are not expected to be able to
# customize.
PythonInternalToolsInfo = provider(fields = {
    "default_sitecustomize": Artifact,
})

def _impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        PythonInternalToolsInfo(
            default_sitecustomize = ctx.attrs.default_sitecustomize,
        ),
    ]

python_internal_tools = rule(
    impl = _impl,
    attrs = {
        "default_sitecustomize": attrs.source(default = "prelude//python/tools/make_par:sitecustomize.py"),
    },
    is_toolchain_rule = True,
)
