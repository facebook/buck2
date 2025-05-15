# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":erlang_build.bzl", "BuildEnvironment", "erlang_build")
load(":erlang_info.bzl", "ErlangAppIncludeInfo")
load(
    ":erlang_toolchain.bzl",
    "select_toolchains",
)
load(
    ":erlang_utils.bzl",
    "multidict_projection_key",
)

def erlang_application_includes_impl(ctx: AnalysisContext) -> list[Provider]:
    """ rule for application includes target
    """

    # prepare include directory for current app
    name = ctx.attrs.app_name

    toolchains = select_toolchains(ctx)
    build_environments = {}
    for toolchain in toolchains.values():
        build_environment = BuildEnvironment(
            includes = {},
            include_dirs = {},
            deps_files = {},
        )
        erlang_build.build_steps.generate_include_artifacts(
            ctx,
            toolchain,
            build_environment,
            name,
            ctx.attrs.includes,
        )
        build_environments[toolchain.name] = build_environment

    # build application info
    app_include_info = ErlangAppIncludeInfo(
        name = name,
        includes = multidict_projection_key(build_environments, "includes", name),
        include_dir = multidict_projection_key(build_environments, "include_dirs", name),
        deps_files = multidict_projection_key(build_environments, "deps_files", name),
        _original_includes = ctx.attrs.includes,
    )

    return [
        DefaultInfo(),
        app_include_info,
    ]
