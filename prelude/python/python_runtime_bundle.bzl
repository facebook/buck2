# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
A bundled runtime includes the full install of python in the par, allowing a fully hermetic distribution.
This rule defines the artifacts which may be include.
include - the headers files required for compiling against the runtime
libpython - the shared object `libpython.so` which has all of the required symbols for the runtime
py_bin - the python executable
py_version - a string denoting the version of python represented by this provider
stdlib - the path to the python standard library
"""
PythonRuntimeBundleInfo = provider(fields = {
    "include": provider_field(Artifact),
    "libpython": provider_field(Artifact | None),
    "py_bin": provider_field(Artifact),
    "py_version": provider_field(str),
    "stdlib": provider_field(Artifact),
})

def python_runtime_bundle_impl(ctx: AnalysisContext) -> list[Provider]:
    root = ctx.attrs.install_root[DefaultInfo].default_outputs[0]
    info = PythonRuntimeBundleInfo(
        py_version = ctx.attrs.py_version,
        py_bin = root.project(ctx.attrs.py_bin),
        stdlib = root.project(ctx.attrs.stdlib),
        libpython = root.project(ctx.attrs.libpython) if ctx.attrs.libpython else None,
        include = root.project(ctx.attrs.include),
    )
    return [DefaultInfo(default_output = root), info]
