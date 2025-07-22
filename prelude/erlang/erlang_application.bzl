# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load(
    ":erlang_build.bzl",
    "BuildEnvironment",  # @unused Used as type
    "erlang_build",
)
load(
    ":erlang_dependencies.bzl",
    "ErlAppDependencies",
    "erlang_deps_rule",
)
load(
    ":erlang_info.bzl",
    "ErlangAppIncludeInfo",
    "ErlangAppInfo",
    "ErlangAppOrTestInfo",
    "ErlangDependencyInfo",
)
load(":erlang_shell.bzl", "erlang_shell")
load(
    ":erlang_toolchain.bzl",
    "Toolchain",  # @unused Used as type
    "get_toolchain",
)
load(
    ":erlang_utils.bzl",
    "app_name",
)

StartDependencySet = transitive_set()

StartTypeValues = ["permanent", "transient", "temporary", "load", "none"]

StartType = enum(*StartTypeValues)

StartSpec = record(
    name = field(str),
    version = field(str),
    resolved = field(bool),
    start_type = field(StartType),
)

BuiltApplication = record(
    build_environment = field(BuildEnvironment),
    app_file = field(Artifact),
    priv_dir = field(Artifact),
)

def erlang_application_impl(ctx: AnalysisContext) -> Promise:
    # select the correct tools from the toolchain
    toolchain = get_toolchain(ctx)

    # collect all dependencies
    dep_info = ctx.actions.anon_target(erlang_deps_rule, {
        "applications": ctx.attrs.applications,
        "extra_includes": ctx.attrs.extra_includes,
        "included_applications": ctx.attrs.included_applications,
    })

    name = app_name(ctx)

    return dep_info.promise.map(lambda dep_info: build_application(ctx, name, toolchain, dep_info[ErlangDependencyInfo]))

def build_application(ctx: AnalysisContext, name: str, toolchain: Toolchain, dep_info: ErlangDependencyInfo) -> Promise:
    if name in dep_info.dependencies and ErlangAppInfo in dep_info.dependencies[name]:
        fail("cannot depend on an application with the same name: %s" % (dep_info.dependencies[name].label,))

    result = _build_erlang_application(ctx, name, toolchain, dep_info)
    build_environment = result.build_environment

    # link final output
    app_folder = link_output(
        ctx,
        name,
        result,
    )

    # build start dependencies in reverse order
    start_dependencies = _build_start_dependencies(ctx)

    app_info = build_app_info(
        ctx,
        dep_info.dependencies,
        build_environment,
        app_folder,
        start_dependencies,
    )

    # generate DefaultInfo and RunInfo providers
    default_info = _build_default_info(dep_info, app_folder)
    run_info = erlang_shell.build_run_info(
        ctx,
        dep_info = dep_info,
        additional_code_path = cmd_args(app_folder, format = "{}/ebin"),
    )
    return run_info.map(lambda run_info: [default_info, run_info, app_info, ErlangAppOrTestInfo()])

def _build_erlang_application(ctx: AnalysisContext, name: str, toolchain: Toolchain, dep_info: ErlangDependencyInfo) -> BuiltApplication:
    include_info = None
    if ctx.attrs._includes_target:
        include_info = ctx.attrs._includes_target[ErlangAppIncludeInfo]
        if include_info._original_includes != ctx.attrs.includes:
            fail("includes of the includes_target and direct includes must be the same, got {} and {}".format(include_info._original_includes, ctx.attrs.includes))
        if include_info.name != name:
            fail("includes_target must have the same name as the application, got {} and {}".format(include_info.name, name))
    build_environment = erlang_build.prepare_build_environment(dep_info, include_info)

    # build generated inputs
    generated_source_artifacts = erlang_build.build_steps.generated_source_artifacts(ctx, toolchain, name)

    # collect all inputs
    src_artifacts = [
        src
        for src in ctx.attrs.srcs
        if erlang_build.utils.is_erl(src) and erlang_build.utils.module_name(src) not in generated_source_artifacts
    ] + generated_source_artifacts.values()

    private_header_artifacts = [header for header in ctx.attrs.srcs if erlang_build.utils.is_hrl(header)]

    # build output artifacts

    # public includes only triggered if this won't called from erlang_application macro
    # and includes weren't redirected to the includes_target dependency
    if not include_info:
        erlang_build.build_steps.generate_include_artifacts(
            ctx,
            toolchain,
            build_environment,
            name,
            ctx.attrs.includes,
        )

    # private includes
    erlang_build.build_steps.generate_include_artifacts(
        ctx,
        toolchain,
        build_environment,
        name,
        private_header_artifacts,
        is_private = True,
    )

    # maybe peek private includes
    erlang_build.utils.peek_private_includes(
        ctx,
        build_environment,
    )

    # beams
    erlang_build.build_steps.generate_beam_artifacts(
        ctx,
        toolchain,
        build_environment,
        name,
        src_artifacts,
    )

    # create <appname>.app file
    app_file = _generate_app_file(
        ctx,
        toolchain,
        name,
        src_artifacts,
    )

    # priv
    priv_dir = _generate_priv_dir(ctx)

    return BuiltApplication(
        build_environment = build_environment,
        app_file = app_file,
        priv_dir = priv_dir,
    )

def _generate_priv_dir(ctx: AnalysisContext) -> Artifact:
    """Generate the application's priv dir."""
    name = app_name(ctx)

    resources = ctx.attrs.resources
    priv_symlinks = {}
    for resource in resources:
        for file in resource[DefaultInfo].default_outputs:
            priv_symlinks[file.short_path] = file
        for file in resource[DefaultInfo].other_outputs:
            if isinstance(file, Artifact):
                priv_symlinks[file.short_path] = file

    return ctx.actions.symlinked_dir(
        paths.join(
            erlang_build.utils.BUILD_DIR,
            name,
            "priv",
        ),
        priv_symlinks,
    )

def _generate_app_file(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        name: str,
        srcs: list[Artifact]) -> Artifact:
    """ rule for generating the .app files

    NOTE: We are using the .erl files as input to avoid dependencies on
          beams.
    """
    _check_application_dependencies(ctx)

    app_file_name = name + ".app"
    output = ctx.actions.declare_output(erlang_build.utils.BUILD_DIR, app_file_name)
    app_info_file = _app_info_content(ctx, name, srcs)

    erlang_build.utils.run_with_env(
        ctx,
        toolchain,
        cmd_args(toolchain.app_src_script, app_info_file, output.as_output()),
        category = "app_resource",
        identifier = name,
    )

    return output

def _check_application_dependencies(ctx: AnalysisContext):
    """ there must not be duplicated applications within applications and included_applications
    """
    discovered = _check_applications_field(ctx.attrs.applications, "applications", {})
    _check_applications_field(ctx.attrs.included_applications, "included_applications", discovered)

def _check_applications_field(field: list[Dependency], tag: str, discovered: dict[str, str]) -> dict[str, str]:
    for application in field:
        name = application[ErlangAppInfo].name
        if name in discovered:
            fail("discovered {} in `{}`, but the application was already specified in `{}`. The `applications` and `included_applications` field must be unique.".format(name, tag, discovered[name]))
        else:
            discovered[name] = tag
    return discovered

def _app_info_content(
        ctx: AnalysisContext,
        name: str,
        srcs: list[Artifact]) -> Artifact:
    """build an app_info.json file that contains the meta information for building the .app file"""
    app_info = ctx.actions.declare_output(erlang_build.utils.BUILD_DIR, "app_info.json")

    data = {
        "applications": [
            app[ErlangAppInfo].name
            for app in ctx.attrs.applications
        ],
        "included_applications": [
            app[ErlangAppInfo].name
            for app in ctx.attrs.included_applications
        ],
        "name": name,
        "sources": srcs,
    }
    if ctx.attrs.version:
        data["version"] = ctx.attrs.version
    if ctx.attrs.app_src:
        data["template"] = ctx.attrs.app_src
        app_info = app_info.with_associated_artifacts([ctx.attrs.app_src])
    if ctx.attrs.mod:
        data["mod"] = ctx.attrs.mod
    if ctx.attrs.env:
        data["env"] = ctx.attrs.env
    if ctx.attrs.extra_properties:
        data["metadata"] = ctx.attrs.extra_properties

    ctx.actions.write_json(app_info.as_output(), data)
    return app_info

def link_output(
        ctx: AnalysisContext,
        link_path: str,
        built: BuiltApplication) -> Artifact:
    """Link application output folder in working dir root folder."""
    name = app_name(ctx)

    build_environment = built.build_environment
    ebin = build_environment.beams.get(name, {}).values() + [built.app_file]
    ebin = {
        paths.join("ebin", ebin_file.basename): ebin_file
        for ebin_file in ebin
    }

    srcs = _link_srcs_folder(ctx)

    link_spec = {}
    link_spec.update(ebin)
    link_spec.update(srcs)
    link_spec["priv"] = built.priv_dir
    if name in build_environment.include_dirs:
        link_spec["include"] = build_environment.include_dirs[name]

    return ctx.actions.symlinked_dir(link_path, link_spec)

def _link_srcs_folder(ctx: AnalysisContext) -> dict[str, Artifact]:
    """Build mapping for the src folder if erlang.include_src is set"""
    if not ctx.attrs.include_src:
        return {}
    srcs = {
        paths.join("src", src_file.basename): src_file
        for src_file in ctx.attrs.srcs
    }
    if ctx.attrs.app_src:
        srcs[paths.join("src", ctx.attrs.app_src.basename)] = ctx.attrs.app_src
    return srcs

def _build_start_dependencies(ctx: AnalysisContext) -> list[StartDependencySet]:
    return build_apps_start_dependencies(
        ctx,
        [(app, StartType("permanent")) for app in ctx.attrs.applications],
    ) + build_apps_start_dependencies(
        ctx,
        [(app, StartType("load")) for app in ctx.attrs.included_applications],
    )

def build_apps_start_dependencies(ctx: AnalysisContext, apps: list[(Dependency, StartType)]) -> list[StartDependencySet]:
    start_dependencies = []
    for app, start_type in apps[::-1]:
        app_spec = _build_start_spec(app[ErlangAppInfo], start_type)

        if app[ErlangAppInfo].virtual:
            children = []
        else:
            children = app[ErlangAppInfo].start_dependencies

        app_set = ctx.actions.tset(
            StartDependencySet,
            value = app_spec,
            children = children,
        )

        start_dependencies.append(app_set)

    return start_dependencies

def _build_start_spec(app_info: Provider, start_type: StartType) -> StartSpec:
    return StartSpec(
        name = app_info.name,
        version = app_info.version,
        resolved = not app_info.virtual,
        start_type = start_type,
    )

def _build_default_info(dep_info: ErlangDependencyInfo, app_dir: Artifact) -> Provider:
    """ generate default_outputs and DefaultInfo provider
    """

    # We depend on the code path of all dependencies to force them to be compiled
    # and emit errors when users compile just this one application
    return DefaultInfo(default_output = app_dir, other_outputs = [dep_info.code_path])

def build_app_info(
        ctx: AnalysisContext,
        dependencies: ErlAppDependencies,
        build_environment: BuildEnvironment,
        app_folder: Artifact,
        start_dependencies: list[StartDependencySet]) -> Provider:
    name = app_name(ctx)

    # build application info
    return ErlangAppInfo(
        name = name,
        version = ctx.attrs.version,
        beams = build_environment.beams.get(name),
        dependencies = dependencies,
        start_dependencies = start_dependencies,
        includes = build_environment.includes.get(name),
        include_dir = build_environment.include_dirs.get(name),
        private_includes = build_environment.private_includes.get(name),
        private_include_dir = build_environment.private_include_dirs.get(name),
        header_deps_file = build_environment.header_deps_files.get(name),
        virtual = False,
        app_folder = app_folder,
    )
