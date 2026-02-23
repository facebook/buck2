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

    # Validate include_erts configuration
    _validate_include_erts(ctx, toolchain)

    # OTP base structure
    lib_dir = build_lib_dir(ctx, apps)

    # erts
    maybe_erts = _build_erts(ctx, toolchain)

    maybe_boot_scripts = _build_boot_scripts(ctx, toolchain, lib_dir["lib"])

    # start_erl.data for releases with bundled ERTS
    maybe_start_erl_data = _build_start_erl_data(ctx, toolchain)

    # release specific variables in bin/release_variables
    release_variables = _build_release_variables(ctx, toolchain)

    # Overlays
    overlays = _build_overlays(ctx)

    # link output
    all_outputs = {}
    for outputs in [
        lib_dir,
        maybe_boot_scripts,
        maybe_start_erl_data,
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
        (dep[ErlangAppInfo].name + "-" + dep[ErlangAppInfo].version): dep[ErlangAppInfo].app_folder
        for dep in all_apps.values()
        if ErlangAppInfo in dep and
           (include_erts or not dep[ErlangAppInfo].virtual)
    }

    lib_dir = ctx.actions.symlinked_dir(
        paths.join(erlang_build.utils.BUILD_DIR, "lib"),
        link_spec,
    )
    return {"lib": lib_dir}

def _build_boot_scripts(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        lib_dir: Artifact) -> dict[str, Artifact]:
    link_spec = {}

    if ctx.attrs.generate_default_bootscript:
        maybe_default_boot_script = _build_default_boot_scripts(ctx, toolchain, lib_dir)
        link_spec.update(maybe_default_boot_script)

    # write applications spec to file
    data = [
        _app_info_to_data(app_info)
        for app_info in ctx.attrs.applications
    ]
    spec_file = ctx.actions.write_json(
        paths.join(erlang_build.utils.BUILD_DIR, "bootscripts", "applications_json"),
        data,
    )

    for script_name, builder in ctx.attrs.bootscript_builders.items():
        builder_args = builder[RunInfo].args
        custom_boot_script_spec = _build_custom_boot_scripts(ctx, toolchain, spec_file, script_name, builder_args, lib_dir)
        link_spec.update(custom_boot_script_spec)

    return link_spec

def _build_default_boot_scripts(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        lib_dir: Artifact) -> dict[str, Artifact]:
    """Build Name.rel, start.script, and start.boot in the release folder.

    Boot scripts are always generated regardless of include_erts setting.
    When include_erts=False (default), OTP applications use runtime version discovery.
    When include_erts=True, explicit versions from the toolchain are used and additional
    no_dot_erlang boot scripts are generated for the self-contained release.
    """
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
    for app_name in root_apps_names[::-1]:
        release_applications.append(root_apps_spec[app_name])

    data = {
        "apps": release_applications[::-1],
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

    # Always include the standard boot scripts
    boot_files = [
        "{}.rel".format(release_name),
        "start.script",
        "start.boot",
    ]

    # Only include no_dot_erlang boot scripts for self-contained releases with bundled ERTS
    if ctx.attrs.include_erts:
        boot_files.extend([
            "no_dot_erlang.script",
            "no_dot_erlang.boot",
        ])

    return {
        paths.join("releases", ctx.attrs.version, file): scripts_dir.project(file)
        for file in boot_files
    }

def _build_custom_boot_scripts(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        spec_file: Artifact,
        script_name: str,
        builder: cmd_args,
        lib_dir: Artifact) -> dict[str, Artifact]:
    boot_script = ctx.actions.declare_output(paths.join(erlang_build.utils.BUILD_DIR, "bootscripts", script_name))
    raw_script_name = paths.replace_extension(script_name, ".script")
    raw_script = ctx.actions.declare_output(paths.join(erlang_build.utils.BUILD_DIR, "bootscripts", raw_script_name))

    erlang_build.utils.run_with_env(
        ctx,
        toolchain,
        cmd_args(builder, spec_file, lib_dir, boot_script.as_output(), raw_script.as_output()),
        category = "build_custom_boot_script",
        identifier = script_name,
    )

    return {
        paths.join("releases", ctx.attrs.version, script_name): boot_script,
        paths.join("releases", ctx.attrs.version, raw_script_name): raw_script,
    }

def _app_info_to_data(app_info: Dependency | (Dependency, str)) -> (str, str):
    if type(app_info) == "tuple":
        app_info, start_type = app_info
    else:
        start_type = "permanent"

    erlang_app = app_info[ErlangAppInfo]
    return (erlang_app.name, start_type)

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
            "erts-{}".format(toolchain.erts_toolchain_info.erts_version),
        ),
        toolchain.erts_toolchain_info.output,
    )

    return {"erts-{}".format(toolchain.erts_toolchain_info.erts_version): erts_dir}

def _build_start_erl_data(
        ctx: AnalysisContext,
        toolchain: Toolchain) -> dict[str, Artifact]:
    """Generate start_erl.data file for releases with bundled ERTS.

    This file contains the ERTS version and release version,
    used by the release boot scripts to determine which ERTS and
    release to start.

    Format: <ERTS_VERSION> <RELEASE_VERSION>
    Example: 15.1 1.0.0
    """
    if not ctx.attrs.include_erts:
        return {}

    content = "{} {}\n".format(
        toolchain.erts_toolchain_info.erts_version,
        ctx.attrs.version,
    )

    start_erl_data = ctx.actions.write(
        paths.join(erlang_build.utils.BUILD_DIR, "start_erl.data"),
        content,
    )

    return {"releases/start_erl.data": start_erl_data}

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

def _validate_include_erts(ctx: AnalysisContext, toolchain: Toolchain) -> None:
    """Validate that include_erts is properly configured with required version information"""
    if not ctx.attrs.include_erts:
        return

    # Check if applications list is empty (dynamic mode)
    if not toolchain.erts_toolchain_info.applications:
        fail("""
ERROR: include_erts=True requires explicit OTP application versions in your erlang_toolchain.

Currently, your erlang_toolchain does not have the 'applications' attribute configured,
which is required for creating self-contained releases with bundled ERTS.

To fix this:

1. Generate OTP version information from your Erlang installation:

   $ python3 buck2/prelude/erlang/toolchain/generate_otp_versions.py my_otp_versions.bzl

2. Commit the generated file and load it in your BUCK file:

   load(":my_otp_versions.bzl", "get_otp_applications", "get_erts_version")

3. Configure your erlang_toolchain with the application versions:

   erlang_toolchain(
       name = "my-toolchain",
       applications = get_otp_applications(),
       erts_version = get_erts_version(),
       otp_binaries = "...",
       # ... other configuration
   )

Alternatively, if you don't need a self-contained release with bundled ERTS,
set include_erts=False (or remove it, as False is the default).

Documentation: https://buck2.build/docs/prelude/erlang/
Target: {target}
""".format(target = str(ctx.label)))

    # Check if erts_version is still dynamic
    if toolchain.erts_toolchain_info.erts_version == "dynamic":
        fail("""
ERROR: include_erts=True requires an explicit erts_version in your erlang_toolchain.

Current erts_version is 'dynamic' which only works when include_erts=False.

Please ensure you've configured your erlang_toolchain with:
  - applications = get_otp_applications()  # from generated .bzl file
  - erts_version = get_erts_version()      # from generated .bzl file

See the error message above for how to generate the version configuration.

Target: {target}
""".format(target = str(ctx.label)))
