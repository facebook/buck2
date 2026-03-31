# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

CythonToolchainInfo = provider(fields = {
    # The cython compiler binary (RunInfo provider).
    # Python version-based selection should be handled via select() + py_version_select()
    # on the compiler attribute in the toolchain BUCK definition, mirroring the
    # fbsource//third-party/pypi/cython:compiler alias pattern.
    "compiler": provider_field(RunInfo),
    # Default compiler flags. Currently unused by cython_library and
    # cython_static_extension (flags are built from rule attributes instead).
    "default_flags": provider_field(list[str]),
})

def cython_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        CythonToolchainInfo(
            compiler = ctx.attrs.compiler[RunInfo],
            default_flags = ctx.attrs.default_flags,
        ),
    ]
