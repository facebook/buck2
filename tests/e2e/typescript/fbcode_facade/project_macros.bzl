# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@fbcode_macros//build_defs:typescript.bzl",
    _typescript_bundle = "typescript_bundle",
    _typescript_compile = "typescript_compile",
    _typescript_library = "typescript_library",
    _typescript_run = "typescript_run",
)

def project_typescript_binary(name, package_name, srcs, entry_point, bundler, bundler_input_kind, compiler = None, runtime = None, deps = [], **kwargs):
    # A load-time macro cannot inspect the bundler provider, so the kind must match its declaration.
    if bundler_input_kind not in ["sources", "modules"]:
        fail("project_typescript_binary bundler_input_kind must be 'sources' or 'modules'")
    if compiler != None and bundler_input_kind == "sources":
        fail("project_typescript_binary cannot use a compiler with a source-input bundler")

    source_name = name + "-sources"
    bundle_name = name + "-bundle"
    _typescript_library(
        name = source_name,
        package_name = package_name,
        srcs = srcs,
        entry_point = entry_point,
        deps = deps,
        emit_runtime = compiler == None and bundler_input_kind == "modules",
        visibility = [],
    )
    bundle_source = ":" + source_name
    if compiler != None:
        compiled_name = name + "-compiled"
        _typescript_compile(
            name = compiled_name,
            compiler = compiler,
            source = ":" + source_name,
            visibility = [],
        )
        bundle_source = ":" + compiled_name
    _typescript_bundle(
        name = bundle_name,
        bundler = bundler,
        source = bundle_source,
        visibility = [],
    )
    _typescript_run(name = name, program = ":" + bundle_name, runtime = runtime, **kwargs)
