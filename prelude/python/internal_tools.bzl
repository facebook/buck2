# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# This is a provider that acts much like `PythonToolchainInfo` and is used sort of similarly.
# However, `PythonToolchainInfo` is used for things that are intended to actually be customizable
# on a per-toolchain basis. This is for things that users are not expected to be able to
# customize.
PythonInternalToolsInfo = provider(fields = {
    "default_sitecustomize": Artifact,
    "fail_with_message": RunInfo,
    "generate_static_extension_info": Dependency,
    "make_py_package_inplace": RunInfo,
    "make_py_package_manifest_module": RunInfo,
    "make_py_package_modules": RunInfo,
    "make_source_db": RunInfo,
    "make_source_db_no_deps": RunInfo,
    "run_lpar_main": Artifact,
    # A filegroup that gets added to all python executables
    "runtime_library": Dependency,
})

def _impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        PythonInternalToolsInfo(
            default_sitecustomize = ctx.attrs.default_sitecustomize,
            fail_with_message = ctx.attrs.fail_with_message[RunInfo],
            generate_static_extension_info = ctx.attrs.generate_static_extension_info,
            make_source_db = ctx.attrs.make_source_db[RunInfo],
            make_source_db_no_deps = ctx.attrs.make_source_db_no_deps[RunInfo],
            make_py_package_inplace = ctx.attrs.make_py_package_inplace[RunInfo],
            make_py_package_manifest_module = ctx.attrs.make_py_package_manifest_module[RunInfo],
            make_py_package_modules = ctx.attrs.make_py_package_modules[RunInfo],
            run_lpar_main = ctx.attrs.run_lpar_main,
            runtime_library = ctx.attrs.runtime_library,
        ),
    ]

python_internal_tools = rule(
    impl = _impl,
    attrs = {
        "default_sitecustomize": attrs.source(default = "prelude//python/tools/make_par:sitecustomize.py"),
        "fail_with_message": attrs.exec_dep(default = "prelude//python/tools:fail_with_message", providers = [RunInfo]),
        "generate_static_extension_info": attrs.exec_dep(default = "prelude//python/tools:generate_static_extension_info"),
        "make_py_package_inplace": attrs.exec_dep(default = "prelude//python/tools:make_py_package_inplace", providers = [RunInfo]),
        "make_py_package_manifest_module": attrs.exec_dep(default = "prelude//python/tools:make_py_package_manifest_module", providers = [RunInfo]),
        "make_py_package_modules": attrs.exec_dep(default = "prelude//python/tools:make_py_package_modules", providers = [RunInfo]),
        "make_source_db": attrs.exec_dep(default = "prelude//python/tools:make_source_db", providers = [RunInfo]),
        "make_source_db_no_deps": attrs.exec_dep(default = "prelude//python/tools:make_source_db_no_deps", providers = [RunInfo]),
        "run_lpar_main": attrs.source(default = "prelude//python/tools/make_par:__run_lpar_main__.py"),
        "runtime_library": attrs.dep(default = "prelude//python/runtime:bootstrap_files"),
    },
    is_toolchain_rule = True,
)
