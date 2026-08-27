# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as type
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

ReleaseConfig = record(
    # the target the release is built for, only used to point failures at it
    label = Label,
    name = str,
    version = str,
    # the applications the release starts, in order, with the type each is started with, and
    # together with their transitive dependencies what `lib/` is drawn from
    applications = list[(Dependency, StartType)],
    toolchain = Toolchain,
    # the environment every toolchain invocation runs with, `None` for the toolchain's own
    os_env = field(dict[str, str] | None, None),
    include_erts = bool,
    is_executable = bool,
    generate_default_bootscript = bool,
    default_bootscript_name = str,
    bootscript_builders = dict[str, cmd_args],
    extra_bootscript_builder_args = list[ArgLike],
    # the config files the launcher hands the emulator, in the order they are applied, so a later
    # one overrides an earlier one. Each is a path from the release root without the `.config`
    # extension, the form `-config` names a file with, and has to be part of the release.
    config_paths = list[str],
    # artifacts to install, mapping the directory they go into, from the release root, to their contents
    overlays = dict[str, list[Artifact]],
)

def erlang_release_impl(ctx: AnalysisContext) -> list[Provider]:
    config = _release_config(ctx)

    all_outputs = build_release(ctx.actions, config)
    release_dir = _symlink_primary_toolchain_output(ctx.actions, config, all_outputs)
    providers = [DefaultInfo(default_output = release_dir), ErlangReleaseInfo(name = config.name)]

    if config.is_executable:
        launcher = release_dir.project(_launcher_path(config))
        providers.append(RunInfo(cmd_args(launcher)))

    return providers

def _release_config(ctx: AnalysisContext) -> ReleaseConfig:
    applications = _applications(ctx)
    toolchain = get_toolchain(ctx)
    overlays = {
        target: [artifact for dep in deps for artifact in dep[DefaultInfo].default_outputs + dep[DefaultInfo].other_outputs]
        for target, deps in ctx.attrs.overlays.items()
    }

    # an `erlang_release` is configured with the one `sys.config` the OTP layout gives it
    sys_config = paths.join("releases", ctx.attrs.version, "sys")
    config_paths = [sys_config] if sys_config + ".config" in _overlay_paths(overlays) else []

    return ReleaseConfig(
        label = ctx.label,
        name = ctx.attrs.release_name if ctx.attrs.release_name else ctx.attrs.name,
        version = ctx.attrs.version,
        applications = applications,
        toolchain = toolchain,
        os_env = getattr(ctx.attrs, "os_env", None),
        include_erts = ctx.attrs.include_erts,
        is_executable = ctx.attrs.is_executable,
        generate_default_bootscript = ctx.attrs.generate_default_bootscript,
        default_bootscript_name = ctx.attrs.default_bootscript_name,
        bootscript_builders = {script_name: builder[RunInfo].args for script_name, builder in ctx.attrs.bootscript_builders.items()},
        extra_bootscript_builder_args = ctx.attrs.extra_bootscript_builder_args,
        config_paths = config_paths,
        overlays = overlays,
    )

def _applications(ctx: AnalysisContext) -> list[(Dependency, StartType)]:
    """Extract the applications, with their start type, from the `applications` field, order preserving"""
    applications = []
    for dep in ctx.attrs.applications:
        if type(dep) == "tuple":
            applications.append((dep[0], StartType(dep[1])))
        else:
            applications.append((dep, StartType("permanent")))
    return applications

def build_release(actions: AnalysisActions, config: ReleaseConfig) -> dict[str, Artifact]:
    """Build the contents of an OTP release, mapping each path from the release root to its artifact.

    The output paths it declares are fixed, so one analysis can only build one release.
    """

    # Validate include_erts configuration
    _validate_include_erts(config)
    _validate_is_executable(config)

    # OTP base structure
    lib_dir = _build_lib_dir(
        actions,
        flatten_dependencies([app for app, _ in config.applications]),
        config.include_erts,
    )

    # erts
    maybe_erts = _build_erts(actions, config)

    maybe_boot_scripts = _build_boot_scripts(actions, config, lib_dir["lib"])

    # start_erl.data for releases with bundled ERTS
    maybe_start_erl_data = _build_start_erl_data(actions, config)

    # release specific variables in bin/release_variables
    release_variables = _build_release_variables(actions, config)

    # Overlays
    overlays = _build_overlays(config.overlays)

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

    # bin/<release_name> for runnable releases, last because it reads what the release contains
    launcher = _build_launcher(actions, config, all_outputs)
    for link_path in launcher:
        if link_path in all_outputs:
            fail("the launcher of %s is installed at %s, which the release already contains" % (str(config.label), link_path))
    all_outputs.update(launcher)

    return all_outputs

def build_lib_dir(ctx: AnalysisContext, all_apps: ErlAppDependencies) -> dict[str, Artifact]:
    """Build lib dir according to OTP specifications.

    .. seealso:: `OTP Design Principles Release Structure <https://www.erlang.org/doc/design_principles/release_structure.html>`_
    """
    include_erts = False
    if "include_erts" in dir(ctx.attrs):
        include_erts = ctx.attrs.include_erts

    return _build_lib_dir(ctx.actions, all_apps, include_erts)

def _build_lib_dir(actions: AnalysisActions, all_apps: ErlAppDependencies, include_erts: bool) -> dict[str, Artifact]:
    link_spec = {
        (dep[ErlangAppInfo].name + "-" + dep[ErlangAppInfo].version): dep[ErlangAppInfo].app_folder
        for dep in all_apps.values()
        if ErlangAppInfo in dep and (include_erts or not dep[ErlangAppInfo].virtual)
    }

    lib_dir = actions.symlinked_dir(
        paths.join(erlang_build.utils.BUILD_DIR, "lib"),
        link_spec,
        has_content_based_path = False,
    )
    return {"lib": lib_dir}

def _build_boot_scripts(actions: AnalysisActions, config: ReleaseConfig, lib_dir: Artifact) -> dict[str, Artifact]:
    link_spec = {}

    if config.generate_default_bootscript:
        maybe_default_boot_script = _build_default_boot_scripts(actions, config, lib_dir)
        link_spec.update(maybe_default_boot_script)

    # write applications spec to file
    data = [(app[ErlangAppInfo].name, start_type.value) for app, start_type in config.applications]
    spec_file = actions.write_json(
        paths.join(erlang_build.utils.BUILD_DIR, "bootscripts", "applications_json"),
        data,
        has_content_based_path = False,
    )

    for script_name, builder in config.bootscript_builders.items():
        custom_boot_script_spec = _build_custom_boot_scripts(actions, config, spec_file, script_name, builder, lib_dir)
        link_spec.update(custom_boot_script_spec)

    return link_spec

def _build_default_boot_scripts(actions: AnalysisActions, config: ReleaseConfig, lib_dir: Artifact) -> dict[str, Artifact]:
    """Build Name.rel, start.script, and start.boot in the release folder.

    Boot scripts are always generated regardless of include_erts setting.
    When include_erts=False (default), OTP applications use runtime version discovery.
    When include_erts=True, explicit versions from the toolchain are used and additional
    no_dot_erlang boot scripts are generated for the self-contained release.
    """
    release_name = config.name

    root_apps_names = [app[ErlangAppInfo].name for app, _ in config.applications]
    start_dependencies = build_apps_start_dependencies(actions, config.applications)

    root_set = actions.tset(
        StartDependencySet,
        value = StartSpec(
            name = "__ignored__",
            version = config.version,
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
        "version": config.version,
    }

    spec_file = actions.write_json(paths.join(erlang_build.utils.BUILD_DIR, "boot_script_spec.json"), data, with_inputs = True, has_content_based_path = False)

    scripts_dir = actions.declare_output(erlang_build.utils.BUILD_DIR, "scripts", dir = True, has_content_based_path = False)

    _run_with_env(
        actions,
        config,
        cmd_args(config.toolchain.boot_script_builder, spec_file, scripts_dir.as_output()),
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
    if config.include_erts:
        boot_files.extend([
            "no_dot_erlang.script",
            "no_dot_erlang.boot",
        ])

    result = {paths.join("releases", config.version, file): scripts_dir.project(file) for file in boot_files}

    # Place OTP's boot files in bin/ so erl can find them at ROOTDIR/bin/.
    # When erl runs from bundled ERTS (erts-VSN/bin/erl), it resolves ROOTDIR
    # to the release root and looks for bin/<name>.boot for boot files.
    # These are extracted once per toolchain (not per release) and contain only
    # kernel+stdlib, so that:
    #   - `erl` bare gives a clean shell (uses bin/start.boot)
    #   - `erl -boot no_dot_erlang` works for ectl and other tools
    # mini_start explicitly uses releases/VERSION/start.boot for service startup.
    if config.include_erts:
        result[paths.join("bin", "start.boot")] = config.toolchain.erts_toolchain_info.otp_start_boot
        result[paths.join("bin", "no_dot_erlang.boot")] = config.toolchain.erts_toolchain_info.otp_no_dot_erlang_boot

    return result

def _build_custom_boot_scripts(
    actions: AnalysisActions, config: ReleaseConfig, spec_file: Artifact, script_name: str, builder: cmd_args, lib_dir: Artifact
) -> dict[str, Artifact]:
    boot_script = actions.declare_output(paths.join(erlang_build.utils.BUILD_DIR, "bootscripts", script_name), has_content_based_path = False)
    raw_script_name = paths.replace_extension(script_name, ".script")
    raw_script = actions.declare_output(paths.join(erlang_build.utils.BUILD_DIR, "bootscripts", raw_script_name), has_content_based_path = False)

    _run_with_env(
        actions,
        config,
        cmd_args(
            builder,
            spec_file,
            lib_dir,
            boot_script.as_output(),
            raw_script.as_output(),
            config.extra_bootscript_builder_args,
        ),
        category = "build_custom_boot_script",
        identifier = script_name,
    )

    return {
        paths.join("releases", config.version, script_name): boot_script,
        paths.join("releases", config.version, raw_script_name): raw_script,
    }

def _build_overlays(overlays: dict[str, list[Artifact]]) -> dict[str, Artifact]:
    installed = {}
    for target, artifacts in overlays.items():
        for artifact in artifacts:
            link_path = _overlay_path(target, artifact)
            if link_path in installed:
                fail("multiple overlays defined for the same location: %s" % (link_path,))
            installed[link_path] = artifact
    return installed

def _overlay_paths(overlays: dict[str, list[Artifact]]) -> list[str]:
    return [_overlay_path(target, artifact) for target, artifacts in overlays.items() for artifact in artifacts]

def _overlay_path(target: str, artifact: Artifact) -> str:
    return paths.normalize(paths.join(target, artifact.basename))

def _build_release_variables(actions: AnalysisActions, config: ReleaseConfig) -> dict[str, Artifact]:
    release_name = config.name

    short_path = "bin/release_variables"
    release_variables = actions.declare_output(
        erlang_build.utils.BUILD_DIR,
        "release_variables",
        has_content_based_path = False,
    )

    spec_file = actions.write_json(
        paths.join(erlang_build.utils.BUILD_DIR, "relvars.json"),
        {
            "REL_NAME": release_name,
            "REL_VSN": config.version,
        },
        has_content_based_path = False,
    )

    _run_with_env(
        actions,
        config,
        cmd_args(config.toolchain.release_variables_builder, spec_file, release_variables.as_output()),
        category = "build_release_variables",
        identifier = release_name,
    )
    return {short_path: release_variables}

def _build_launcher(actions: AnalysisActions, config: ReleaseConfig, release_files: dict[str, Artifact]) -> dict[str, Artifact]:
    """Generate bin/<release_name>, a launcher booting the release with the bundled emulator.

    Everything the emulator is told is resolved here rather than at runtime: the erts version, so
    the launcher addresses `erts-<version>` directly, and the boot script, `vm.args` and the config
    files, so the launcher does not depend on what it was invoked as. Every path is relative to
    ROOTDIR, so the release stays relocatable. The tool name is still taken from the launcher's own
    basename, so one release can serve several tools that differ only in the arguments they get.
    """
    if not config.is_executable:
        return {}

    boot_script = paths.join("releases", config.version, config.default_bootscript_name)
    vm_args = boot_script + ".vm.args"

    for config_path in config.config_paths:
        if config_path + ".config" not in release_files:
            fail("%s is configured with `%s.config`, which none of its overlays installs" % (str(config.label), config_path))

    if boot_script + ".boot" not in release_files:
        fail("%s boots with `%s.boot`, which the release does not contain" % (str(config.label), boot_script))

    lines = [
        "#!/usr/bin/env bash",
        "set -euo pipefail",
        # macOS has no `readlink -f`, so the symlink chain is followed one hop at a time
        'SELF="${BASH_SOURCE[0]}"',
        "HOPS=0",
        "while :; do",
        '    ROOTDIR="$(cd -P "$(dirname "$SELF")/.." && pwd)"',
        '    if [ -e "$ROOTDIR/{}.boot" ]; then'.format(boot_script),
        "        break",
        "    fi",
        '    if [ ! -L "$SELF" ]; then',
        '        echo "$0: cannot find the release root above $SELF" >&2',
        "        exit 1",
        "    fi",
        "    HOPS=$((HOPS + 1))",
        "    if [ $HOPS -gt 40 ]; then",
        '        echo "$0: too many symlink hops resolving release root" >&2',
        "        exit 1",
        "    fi",
        '    SELFDIR="$(cd -P "$(dirname "$SELF")" && pwd)"',
        '    SELF="$(readlink "$SELF")"',
        '    case "$SELF" in',
        "        /*) ;;",
        '        *) SELF="$SELFDIR/$SELF" ;;',
        "    esac",
        "done",
        'BINDIR="$ROOTDIR/erts-{}/bin"'.format(config.toolchain.erts_toolchain_info.erts_version),
        'TOOL="$(basename "$0")"',
        "export ROOTDIR BINDIR",
        'exec "$BINDIR/erlexec" \\',
        '    -boot "$ROOTDIR/{}" \\'.format(boot_script),
    ]
    if vm_args in release_files:
        lines.append('    -args_file "$ROOTDIR/{}" \\'.format(vm_args))
    for config_path in config.config_paths:
        lines.append('    -config "$ROOTDIR/{}" \\'.format(config_path))
    lines += [
        '    -extra "$TOOL" ${1+"$@"}',
        "",
    ]

    launcher = actions.write(
        paths.join(erlang_build.utils.BUILD_DIR, "launcher", config.name),
        lines,
        is_executable = True,
        has_content_based_path = False,
    )

    return {_launcher_path(config): launcher}

def _launcher_path(config: ReleaseConfig) -> str:
    return paths.join("bin", config.name)

def _build_erts(actions: AnalysisActions, config: ReleaseConfig) -> dict[str, Artifact]:
    if not config.include_erts:
        return {}

    release_name = config.name
    erts_version = config.toolchain.erts_toolchain_info.erts_version

    erts_dir = actions.symlink_file(
        paths.join(
            erlang_build.utils.BUILD_DIR,
            release_name,
            "erts-{}".format(erts_version),
        ),
        config.toolchain.erts_toolchain_info.output,
        has_content_based_path = False,
    )

    return {"erts-{}".format(erts_version): erts_dir}

def _build_start_erl_data(actions: AnalysisActions, config: ReleaseConfig) -> dict[str, Artifact]:
    """Generate start_erl.data file for releases with bundled ERTS.

    This file contains the ERTS version and release version,
    used by the release boot scripts to determine which ERTS and
    release to start.

    Format: <ERTS_VERSION> <RELEASE_VERSION>
    Example: 15.1 1.0.0
    """
    if not config.include_erts:
        return {}

    content = "{} {}\n".format(
        config.toolchain.erts_toolchain_info.erts_version,
        config.version,
    )

    start_erl_data = actions.write(
        paths.join(erlang_build.utils.BUILD_DIR, "start_erl.data"),
        content,
        has_content_based_path = False,
    )

    return {"releases/start_erl.data": start_erl_data}

def _run_with_env(actions: AnalysisActions, config: ReleaseConfig, args: cmd_args, **kwargs):
    """run interface that injects the environment the release's toolchain invocations run with"""
    env = config.os_env if config.os_env != None else config.toolchain.env

    if "env" in kwargs:
        kwargs["env"].update(env)
    else:
        kwargs["env"] = env

    actions.run(args, **kwargs)

def _symlink_primary_toolchain_output(actions: AnalysisActions, config: ReleaseConfig, artifacts: dict[str, Artifact]) -> Artifact:
    return actions.symlinked_dir(
        config.name,
        artifacts,
        has_content_based_path = False,
    )

def _validate_is_executable(config: ReleaseConfig) -> None:
    """Validate that a runnable release ships the emulator its launcher runs"""
    if config.is_executable and not config.include_erts:
        fail("is_executable = True requires include_erts = True, the launcher runs the emulator from the release's own erts folder: %s" % (str(config.label),))

def _validate_include_erts(config: ReleaseConfig) -> None:
    """Validate that include_erts is properly configured with required version information"""
    if not config.include_erts:
        return

    # Check if applications list is empty (dynamic mode)
    if not config.toolchain.erts_toolchain_info.applications:
        fail(
            """
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
""".format(target = str(config.label))
        )

    # Check if erts_version is still dynamic
    if config.toolchain.erts_toolchain_info.erts_version == "dynamic":
        fail(
            """
ERROR: include_erts=True requires an explicit erts_version in your erlang_toolchain.

Current erts_version is 'dynamic' which only works when include_erts=False.

Please ensure you've configured your erlang_toolchain with:
  - applications = get_otp_applications()  # from generated .bzl file
  - erts_version = get_erts_version()      # from generated .bzl file

See the error message above for how to generate the version configuration.

Target: {target}
""".format(target = str(config.label))
        )
