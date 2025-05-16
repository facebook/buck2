# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load(
    ":erlang_build.bzl",
    "BuildEnvironment",  # @unused Used as type
    "erlang_build",
)
load(
    ":erlang_dependencies.bzl",
    "ErlAppDependencies",
    "flatten_dependencies",
)
load(
    ":erlang_info.bzl",
    "ErlangAppIncludeInfo",
    "ErlangAppInfo",
)
load(":erlang_shell.bzl", "erlang_shell")
load(
    ":erlang_toolchain.bzl",
    "Toolchain",  # @unused Used as type
    "get_primary",
    "select_toolchains",
)
load(
    ":erlang_utils.bzl",
    "action_identifier",
    "app_name",
    "multidict_projection_key",
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

def erlang_application_impl(ctx: AnalysisContext) -> list[Provider]:
    # select the correct tools from the toolchain
    toolchains = select_toolchains(ctx)

    # collect all dependencies
    all_direct_dependencies = ctx.attrs.applications + ctx.attrs.included_applications + ctx.attrs.extra_includes
    dependencies = flatten_dependencies(ctx, all_direct_dependencies)

    name = app_name(ctx)
    if name in dependencies and ErlangAppInfo in dependencies[name]:
        fail("cannot depend on an application with the same name: %s" % (dependencies[name].label,))

    return build_application(ctx, toolchains, dependencies)

def build_application(ctx, toolchains, dependencies) -> list[Provider]:
    name = app_name(ctx)

    build_environments = {}
    app_folders = {}
    start_dependencies = {}
    for toolchain in toolchains.values():
        result = _build_erlang_application(ctx, toolchain, dependencies)
        build_environments[toolchain.name] = result.build_environment

        # link final output
        app_folders[toolchain.name] = link_output(
            ctx,
            paths.join(
                erlang_build.utils.build_dir(toolchain),
                "linked",
                name,
            ),
            result,
        )

        # build start dependencies in reverse order
        start_dependencies[toolchain.name] = _build_start_dependencies(ctx, toolchain)

    primary_app_folder = ctx.actions.symlink_file(name, app_folders[get_primary(ctx)])

    app_info = build_app_info(
        ctx,
        dependencies,
        build_environments,
        app_folders,
        primary_app_folder,
        start_dependencies,
    )

    # generate DefaultInfo and RunInfo providers
    default_info = _build_default_info(dependencies, primary_app_folder)
    run_info = erlang_shell.build_run_info(
        ctx,
        dependencies = dependencies.values(),
        additional_app_paths = [primary_app_folder],
    )
    return [
        default_info,
        run_info,
        app_info,
    ]

def _build_erlang_application(ctx: AnalysisContext, toolchain: Toolchain, dependencies: ErlAppDependencies) -> BuiltApplication:
    name = app_name(ctx)

    include_info = None
    if ctx.attrs._includes_target:
        include_info = ctx.attrs._includes_target[ErlangAppIncludeInfo]
        if include_info._original_includes != ctx.attrs.includes:
            fail("includes of the includes_target and direct includes must be the same, got {} and {}".format(include_info._original_includes, ctx.attrs.includes))
        if include_info.name != name:
            fail("includes_target must have the same name as the application, got {} and {}".format(include_info.name, name))
    build_environment = erlang_build.prepare_build_environment(ctx, toolchain, dependencies, include_info)

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
        toolchain,
        build_environment,
        dependencies,
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
    priv_dir = _generate_priv_dir(
        ctx,
        toolchain,
    )

    return BuiltApplication(
        build_environment = build_environment,
        app_file = app_file,
        priv_dir = priv_dir,
    )

def _generate_priv_dir(
        ctx: AnalysisContext,
        toolchain: Toolchain) -> Artifact:
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
            erlang_build.utils.build_dir(toolchain),
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
    build_dir = erlang_build.utils.build_dir(toolchain)

    app_file_name = name + ".app"
    output = ctx.actions.declare_output(build_dir, app_file_name)
    app_info_file = _app_info_content(ctx, build_dir, name, srcs)

    erlang_build.utils.run_with_env(
        ctx,
        toolchain,
        cmd_args(toolchain.app_src_script, app_info_file, output.as_output()),
        category = "app_resource",
        identifier = action_identifier(toolchain, name),
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
        build_dir: str,
        name: str,
        srcs: list[Artifact]) -> Artifact:
    """build an app_info.json file that contains the meta information for building the .app file"""
    app_info = ctx.actions.declare_output(build_dir, "app_info.json")

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
    ebin = build_environment.beams[name].values() + [built.app_file]
    include = build_environment.include_dirs[name]

    ebin = {
        paths.join("ebin", ebin_file.basename): ebin_file
        for ebin_file in ebin
    }

    srcs = _link_srcs_folder(ctx)

    link_spec = {}
    link_spec.update(ebin)
    link_spec.update(srcs)
    link_spec["include"] = include
    link_spec["priv"] = built.priv_dir

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

def _build_start_dependencies(ctx: AnalysisContext, toolchain: Toolchain) -> list[StartDependencySet]:
    return build_apps_start_dependencies(
        ctx,
        toolchain,
        [(app, StartType("permanent")) for app in ctx.attrs.applications],
    ) + build_apps_start_dependencies(
        ctx,
        toolchain,
        [(app, StartType("load")) for app in ctx.attrs.included_applications],
    )

def build_apps_start_dependencies(ctx: AnalysisContext, toolchain: Toolchain, apps: list[(Dependency, StartType)]) -> list[StartDependencySet]:
    start_dependencies = []
    for app, start_type in apps[::-1]:
        app_spec = _build_start_spec(toolchain, app[ErlangAppInfo], start_type)

        if app[ErlangAppInfo].virtual:
            children = []
        else:
            children = app[ErlangAppInfo].start_dependencies[toolchain.name]

        app_set = ctx.actions.tset(
            StartDependencySet,
            value = app_spec,
            children = children,
        )

        start_dependencies.append(app_set)

    return start_dependencies

def _build_start_spec(toolchain: Toolchain, app_info: Provider, start_type: StartType) -> StartSpec:
    if app_info.version == "dynamic":
        version = app_info.version
    else:
        version = app_info.version[toolchain.name]

    return StartSpec(
        name = app_info.name,
        version = version,
        resolved = not app_info.virtual,
        start_type = start_type,
    )

def _build_default_info(dependencies: ErlAppDependencies, app_dir: Artifact) -> Provider:
    """ generate default_outputs and DefaultInfo provider
    """

    outputs = [
        dep[ErlangAppInfo].app_folder
        for dep in dependencies.values()
        if ErlangAppInfo in dep and
           not dep[ErlangAppInfo].virtual
    ]

    return DefaultInfo(default_output = app_dir, other_outputs = outputs)

def build_app_info(
        ctx: AnalysisContext,
        dependencies: ErlAppDependencies,
        build_environments: dict[str, BuildEnvironment],
        app_folders: dict[str, Artifact],
        primary_app_folder: Artifact,
        start_dependencies: dict[str, list[StartDependencySet]]) -> Provider:
    name = app_name(ctx)

    version = {
        toolchain.name: ctx.attrs.version
        for toolchain in select_toolchains(ctx).values()
    }

    # build application info
    return ErlangAppInfo(
        name = name,
        version = version,
        beams = multidict_projection_key(build_environments, "beams", name),
        dependencies = dependencies,
        start_dependencies = start_dependencies,
        includes = multidict_projection_key(build_environments, "includes", name),
        include_dir = multidict_projection_key(build_environments, "include_dirs", name),
        private_includes = multidict_projection_key(build_environments, "private_includes", name),
        private_include_dir = multidict_projection_key(build_environments, "private_include_dirs", name),
        deps_files = multidict_projection_key(build_environments, "deps_files", name),
        virtual = False,
        app_folders = app_folders,
        app_folder = primary_app_folder,
    )
