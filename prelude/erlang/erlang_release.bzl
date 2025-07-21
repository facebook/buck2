# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load(
    ":erlang_application.bzl",
    "StartDependencySet",
    "StartSpec",
    "StartType",
    "build_apps_start_dependencies",
)
load(":erlang_build.bzl", "erlang_build")
load(":erlang_dependencies.bzl", "ErlAppDependencies", "flatten_dependencies")
load(
    ":erlang_info.bzl",
    "ErlangAppInfo",
    "ErlangReleaseInfo",
)
load(
    ":erlang_toolchain.bzl",
    "Toolchain",  # @unused Used as type
    "get_toolchain",
)

# Erlang Releases according to https://www.erlang.org/doc/design_principles/release_structure.html

def erlang_release_impl(ctx: AnalysisContext) -> list[Provider]:
    apps = flatten_dependencies(_dependencies(ctx))

    all_outputs = _build_release(ctx, apps)
    release_dir = _symlink_primary_toolchain_output(ctx, all_outputs)
    return [DefaultInfo(default_output = release_dir), ErlangReleaseInfo(name = _relname(ctx))]

def _build_release(ctx: AnalysisContext, apps: ErlAppDependencies) -> dict[str, Artifact]:
    toolchain = get_toolchain(ctx)

    # OTP base structure
    lib_dir = build_lib_dir(ctx, apps)

    # erts
    maybe_erts = _build_erts(ctx, toolchain)

    boot_scripts = _build_boot_script(ctx, toolchain, lib_dir["lib"])

    # release specific variables in bin/release_variables
    release_variables = _build_release_variables(ctx, toolchain)

    # Overlays
    overlays = _build_overlays(ctx)

    # link output
    all_outputs = {}
    for outputs in [
        lib_dir,
        boot_scripts,
        overlays,
        release_variables,
        maybe_erts,
    ]:
        all_outputs.update(outputs)

    return all_outputs

def build_lib_dir(
        ctx: AnalysisContext,
        all_apps: ErlAppDependencies) -> dict[str, Artifact]:
    """Build lib dir according to OTP specifications.

    .. seealso:: `OTP Design Principles Release Structure <https://www.erlang.org/doc/design_principles/release_structure.html>`_
    """
    include_erts = False
    if "include_erts" in dir(ctx.attrs):
        include_erts = ctx.attrs.include_erts

    link_spec = {
        dep[ErlangAppInfo].name: dep[ErlangAppInfo].app_folder
        for dep in all_apps.values()
        if ErlangAppInfo in dep and
           (include_erts or not dep[ErlangAppInfo].virtual)
    }

    lib_dir = ctx.actions.symlinked_dir(
        paths.join(erlang_build.utils.BUILD_DIR, "lib"),
        link_spec,
    )
    return {"lib": lib_dir}

def _build_boot_script(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        lib_dir: Artifact) -> dict[str, Artifact]:
    """Build Name.rel, start.script, and start.boot in the release folder."""
    release_name = _relname(ctx)

    start_type_mapping = _dependencies_with_start_types(ctx)
    root_apps = _dependencies(ctx)
    root_apps_names = [app[ErlangAppInfo].name for app in root_apps]

    root_apps_with_start_type = [
        (app, start_type_mapping[_app_name(app)])
        for app in root_apps
    ]
    start_dependencies = build_apps_start_dependencies(ctx, root_apps_with_start_type)

    root_set = ctx.actions.tset(
        StartDependencySet,
        value = StartSpec(
            name = "__ignored__",
            version = ctx.attrs.version,
            start_type = StartType("permanent"),
            resolved = False,
        ),
        children = start_dependencies,
    )

    reverse_start_order = list(root_set.traverse())
    reverse_start_order.pop(0)

    seen = set()
    release_applications = []
    root_apps_spec = {}
    for spec in reverse_start_order[::-1]:
        if spec.name in seen:
            continue
        seen.add(spec.name)

        app_spec = {
            "name": spec.name,
            "resolved": spec.resolved,
            "type": spec.start_type.value,
            "version": spec.version,
        }

        if spec.name in root_apps_names:
            root_apps_spec[spec.name] = app_spec
        else:
            release_applications.append(app_spec)
    for app_name in root_apps_names:
        release_applications.append(root_apps_spec[app_name])

    data = {
        "apps": release_applications,
        "lib_dir": lib_dir,
        "name": release_name,
        "version": ctx.attrs.version,
    }

    spec_file = ctx.actions.write_json(paths.join(erlang_build.utils.BUILD_DIR, "boot_script_spec.json"), data, with_inputs = True)

    scripts_dir = ctx.actions.declare_output(erlang_build.utils.BUILD_DIR, "scripts", dir = True)

    erlang_build.utils.run_with_env(
        ctx,
        toolchain,
        cmd_args(toolchain.boot_script_builder, spec_file, scripts_dir.as_output()),
        category = "build_boot_script",
        identifier = release_name,
    )

    return {
        paths.join("releases", ctx.attrs.version, file): scripts_dir.project(file)
        for file in [
            "{}.rel".format(release_name),
            "start.script",
            "start.boot",
        ]
    }

def _build_overlays(ctx: AnalysisContext) -> dict[str, Artifact]:
    installed = {}
    for target, deps in ctx.attrs.overlays.items():
        for dep in deps:
            for artifact in dep[DefaultInfo].default_outputs + dep[DefaultInfo].other_outputs:
                link_path = paths.normalize(paths.join(target, artifact.basename))
                if link_path in installed:
                    fail("multiple overlays defined for the same location: %s" % (link_path,))
                installed[link_path] = artifact
    return installed

def _build_release_variables(ctx: AnalysisContext, toolchain: Toolchain) -> dict[str, Artifact]:
    release_name = _relname(ctx)

    short_path = "bin/release_variables"
    release_variables = ctx.actions.declare_output(
        erlang_build.utils.BUILD_DIR,
        "release_variables",
    )

    spec_file = ctx.actions.write_json(
        paths.join(erlang_build.utils.BUILD_DIR, "relvars.json"),
        {
            "REL_NAME": release_name,
            "REL_VSN": ctx.attrs.version,
        },
    )

    erlang_build.utils.run_with_env(
        ctx,
        toolchain,
        cmd_args(toolchain.release_variables_builder, spec_file, release_variables.as_output()),
        category = "build_release_variables",
        identifier = release_name,
    )
    return {short_path: release_variables}

def _build_erts(
        ctx: AnalysisContext,
        toolchain: Toolchain) -> dict[str, Artifact]:
    if not ctx.attrs.include_erts:
        return {}

    release_name = _relname(ctx)

    erts_dir = ctx.actions.symlink_file(
        paths.join(
            erlang_build.utils.BUILD_DIR,
            release_name,
            "erts",
        ),
        toolchain.erts,
    )

    return {"erts": erts_dir}

def _symlink_primary_toolchain_output(ctx: AnalysisContext, artifacts: dict[str, Artifact]) -> Artifact:
    return ctx.actions.symlinked_dir(
        _relname(ctx),
        artifacts,
    )

def _relname(ctx: AnalysisContext) -> str:
    return ctx.attrs.release_name if ctx.attrs.release_name else ctx.attrs.name

def _dependencies(ctx: AnalysisContext) -> list[Dependency]:
    """Extract dependencies from `applications` field, order preserving"""
    deps = []
    for dep in ctx.attrs.applications:
        if type(dep) == "tuple":
            deps.append(dep[0])
        else:
            deps.append(dep)
    return deps

def _dependencies_with_start_types(ctx: AnalysisContext) -> dict[str, StartType]:
    """Extract mapping from dependency to start type from `applications` field, this is not order preserving"""
    deps = {}
    for dep in ctx.attrs.applications:
        if type(dep) == "tuple":
            deps[_app_name(dep[0])] = StartType(dep[1])
        else:
            deps[_app_name(dep)] = StartType("permanent")
    return deps

def _app_name(app: Dependency) -> str:
    """Helper to unwrap the name for an erlang application dependency"""
    return app[ErlangAppInfo].name
