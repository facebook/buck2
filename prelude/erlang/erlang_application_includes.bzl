# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":erlang_build.bzl", "BuildEnvironment", "erlang_build")
load(":erlang_info.bzl", "ErlangAppIncludeInfo")
load(":erlang_toolchain.bzl", "get_toolchain")

def erlang_application_includes_impl(ctx: AnalysisContext) -> list[Provider]:
    """ rule for application includes target
    """

    # prepare include directory for current app
    name = ctx.attrs.app_name

    toolchain = get_toolchain(ctx)
    build_environment = BuildEnvironment(
        includes = {},
        include_dirs = {},
        header_deps_files = {},
    )
    erlang_build.build_steps.generate_include_artifacts(
        ctx,
        toolchain,
        build_environment,
        name,
        ctx.attrs.includes,
    )

    # build application info
    app_include_info = ErlangAppIncludeInfo(
        name = name,
        includes = build_environment.includes[name],
        include_dir = build_environment.include_dirs[name],
        header_deps_file = build_environment.header_deps_files[name],
        _original_includes = ctx.attrs.includes,
    )

    return [
        DefaultInfo(),
        app_include_info,
    ]
