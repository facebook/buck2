# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load("@prelude//utils:utils.bzl", "dedupe_by_value")
load(
    ":erlang_build.bzl",
    "erlang_build",
    "module_name",
)
load(":erlang_dependencies.bzl", "erlang_deps_rule")
load(":erlang_info.bzl", "ErlangAppInfo", "ErlangAppOrTestInfo", "ErlangDependencyInfo", "ErlangTestInfo")
load(":erlang_otp_application.bzl", "normalize_application")
load(":erlang_paths.bzl", "basename_without_extension")
load(":erlang_shell.bzl", "erlang_shell")
load(
    ":erlang_toolchain.bzl",
    "get_toolchain",
)
load(
    ":erlang_utils.bzl",
    "file_mapping",
    "preserve_structure",
)

def erlang_tests_macro(
        erlang_app_rule,
        erlang_test_rule,
        suites: list[str],
        deps: list[str] = [],
        resources: list[str] = [],
        property_tests: list[str] = [],
        srcs: list[str] = [],
        prefix: str | None = None,
        generated_app_labels: list[str] = [],
        **common_attributes) -> None:
    """
    Generate multiple erlang_test targets based on the `suites` field.
    Also adds the default 'config' and 'deps' from the buck2 config.
    The macro also produces and adds
    resource targets for files in the suite associated <suitename>_data folder.
    """
    if not suites:
        return

    deps = [normalize_application(dep) for dep in deps]

    if srcs:
        # There is no "good name" for the application
        # We create one using the first suite from the list
        suite_name = basename_without_extension(suites[0])
        srcs_app = "{}_app".format(suite_name)
        app_deps = [dep for dep in deps if not dep.endswith("_SUITE")]
        erlang_app_rule(
            name = srcs_app,
            srcs = srcs,
            labels = generated_app_labels,
            applications = app_deps,
        )
        deps.append(":{}".format(srcs_app))

    if not property_tests:
        first_suite = suites[0]
        prop_target = generate_file_map_target(first_suite, None, "property_test")
        if prop_target:
            property_tests = [prop_target]

    common_attributes["labels"] = common_attributes.get("labels", [])

    common_attributes["labels"] = dedupe_by_value(common_attributes["labels"])

    for suite in suites:
        # forward resources and deps fields and generate erlang_test target
        suite_name = normalize_suite_name(basename_without_extension(suite))
        if not suite_name.endswith("_SUITE"):
            fail("erlang_tests target only accept suite as input, found " + suite_name)

        # check if there is a data folder and add it as resource if existing
        data_dir_name = "{}_data".format(suite_name)
        suite_resource = resources
        data_target = generate_file_map_target(suite, prefix, data_dir_name)
        if data_target:
            suite_resource = list(suite_resource)  # copy
            suite_resource.append(data_target)

        if prefix != None:
            suite_name = "{}_{}".format(prefix, suite_name)

        # forward resources and deps fields and generate erlang_test target
        erlang_test_rule(
            name = suite_name,
            suite = suite,
            deps = deps,
            resources = suite_resource,
            property_tests = property_tests,
            **common_attributes
        )

def normalize_suite_name(suite_name: str) -> str:
    return suite_name.replace(":", "_")

default_test_args = cmd_args(
    "-mode",
    "minimal",
    "-noinput",
    "-noshell",
    "+A0",
    "+S1:1",
    "+sbtu",
    "-run",
    "test_binary",  # provided by ctx.attr._test_binary_lib
    "main",
)

def erlang_test_impl(ctx: AnalysisContext) -> Promise:
    # collect all dependencies
    dep_info_rule = (erlang_deps_rule, {"deps": ctx.attrs.deps + [ctx.attrs._test_binary_lib]})
    binary_lib_dep_info_rule = (erlang_deps_rule, {"deps": [ctx.attrs._test_binary_lib]})

    dep_infos = ctx.actions.anon_targets([dep_info_rule, binary_lib_dep_info_rule])

    return dep_infos.promise.map(lambda dep_infos: _build_erlang_test(
        ctx,
        dep_infos[0][ErlangDependencyInfo],
        dep_infos[1][ErlangDependencyInfo],
    ))

def _build_erlang_test(
        ctx: AnalysisContext,
        dep_info: ErlangDependencyInfo,
        binary_lib_dep_info: ErlangDependencyInfo) -> Promise:
    toolchain = get_toolchain(ctx)
    tools = toolchain.otp_binaries

    # prepare build environment
    build_environment = erlang_build.prepare_build_environment(dep_info)

    erlang_build.utils.peek_private_includes(
        ctx,
        build_environment,
        force_peek = True,
    )

    # Config files for ct
    config_files = [config_file[DefaultInfo].default_outputs[0] for config_file in ctx.attrs.config_files]

    cmd = cmd_args([trampoline[RunInfo] for trampoline in ctx.attrs._trampolines])
    cmd.add(tools.erl, default_test_args)

    app_folders = [
        dep[ErlangAppInfo].app_folder
        for dep in binary_lib_dep_info.dependencies.values()
        if not dep[ErlangAppInfo].virtual
    ]
    cmd.add(cmd_args(app_folders, format = "{}/ebin", prepend = "-pa"))
    cmd.add("--")

    suite = ctx.attrs.suite
    suite_name = module_name(suite)

    erlang_build.build_steps.generate_beam_artifacts(
        ctx,
        toolchain,
        build_environment,
        "tests",
        [suite],
    )

    beam = build_environment.beams["tests"][suite_name]
    ebin_dir = paths.dirname(beam.short_path)

    suite_data = paths.join(ebin_dir, suite_name + "_data")
    data_dir = _build_resource_dir(ctx, ctx.attrs.resources, suite_data)
    property_dir = _build_resource_dir(ctx, ctx.attrs.property_tests, paths.join(ebin_dir, "property_test"))

    output_dir = link_output(ctx, beam, data_dir, property_dir)
    test_info_file = _write_test_info_file(
        ctx = ctx,
        test_suite = suite_name,
        dep_info = dep_info,
        test_dir = output_dir,
        config_files = config_files,
        erl_cmd = toolchain.otp_binaries.erl,
        raw_target = str(ctx.label.raw_target()) if ctx.label else "",
    )
    cmd.add(test_info_file)

    default_info = _build_default_info(dep_info, output_dir)

    # NB. We can't use `quote="shell"` since we need $REPO_ROOT to be expanded by the shell.
    # So we wrap everything in extra double-quotes to protect from spaces in the path
    test_info_file_arg = cmd_args(test_info_file, format = '"<<\\"${REPO_ROOT}/{}\\">>"')

    additional_shell_args = cmd_args(
        cmd_args("-test_cli_lib", "test_info_file", test_info_file_arg, delimiter = " "),
        cmd_args("-eval", ctx.attrs.preamble, quote = "shell", delimiter = " "),
        "-noshell",
    )

    run_info = erlang_shell.build_run_info(
        ctx,
        dep_info = dep_info,
        additional_code_path = cmd_args(output_dir),
        additional_shell_deps = [ctx.attrs._cli_lib],
        additional_args = additional_shell_args,
    )

    re_executor = get_re_executor_from_props(ctx)
    external_runner_info = ExternalRunnerTestInfo(
        type = "erlang_test",
        command = [cmd],
        env = ctx.attrs.env,
        labels = ctx.attrs.labels,
        contacts = ctx.attrs.contacts,
        run_from_project_root = True,
        use_project_relative_paths = True,
        default_executor = re_executor,
    )
    test_info = ErlangTestInfo(
        name = suite_name,
        dependencies = dep_info.dependencies,
        output_dir = output_dir,
    )

    return run_info.map(lambda run_info: [
        default_info,
        run_info,
        ErlangAppOrTestInfo(),
        external_runner_info,
        test_info,
    ])

# Copied from erlang_application.
def _build_default_info(dep_info: ErlangDependencyInfo, output_dir: Artifact) -> Provider:
    """ generate default_outputs and DefaultInfo provider
    """

    # We depend on the code path of all dependencies to force them to be compiled
    # and emit errors when users compile just this one application
    # This was already flattened in erlang_deps_rule
    return DefaultInfo(default_output = output_dir, other_outputs = [dep_info.code_path])

def _write_test_info_file(
        ctx: AnalysisContext,
        test_suite: str,
        dep_info: ErlangDependencyInfo,
        test_dir: Artifact,
        config_files: list[Artifact],
        erl_cmd: [cmd_args, Artifact],
        raw_target: str) -> WriteJsonCliArgs:
    tests_info = {
        "artifact_annotation_mfa": ctx.attrs._artifact_annotation_mfa,
        "common_app_env": ctx.attrs.common_app_env,
        "config_files": config_files,
        "ct_opts": ctx.attrs._ct_opts,
        "dependencies": dep_info.code_path,
        "erl_cmd": erl_cmd,
        "extra_ct_hooks": ctx.attrs.extra_ct_hooks,
        "extra_flags": ctx.attrs.extra_erl_flags,
        "providers": ctx.attrs._providers,
        "raw_target": raw_target,
        "test_dir": test_dir,
        "test_suite": test_suite,
    }
    test_info_file = ctx.actions.declare_output("tests_info")
    return ctx.actions.write_json(test_info_file, tests_info, with_inputs = True)

def _build_resource_dir(ctx: AnalysisContext, resources: list, target_dir: str) -> [Artifact, None]:
    """ build mapping for suite data directory

    generating the necessary mapping information for the suite data directory
    the resulting mapping can be used directly to symlink
    """
    if not resources:
        return None

    include_symlinks = {}
    for resource in resources:
        files = resource[DefaultInfo].default_outputs
        for file in files:
            if file.short_path in include_symlinks:
                fail("duplicate resource file: `{}`, defined in {} and {}".format(file.short_path, include_symlinks[file.short_path], file))
            else:
                include_symlinks[file.short_path] = file
    return ctx.actions.symlinked_dir(
        target_dir,
        include_symlinks,
    )

def link_output(
        ctx: AnalysisContext,
        beam: Artifact,
        data_dir: [Artifact, None],
        property_dir: [Artifact, None]) -> Artifact:
    """Link the data_dirs and the test_suite beam in a single output folder."""
    link_spec = {
        beam.basename: beam,
        ctx.attrs.suite.basename: ctx.attrs.suite,
    }
    if data_dir:
        link_spec[data_dir.basename] = data_dir
    if property_dir:
        link_spec[property_dir.basename] = property_dir
    return ctx.actions.symlinked_dir(ctx.attrs.name, link_spec)

def generate_file_map_target(suite: str, prefix: str | None, dir_name: str) -> str:
    suite_dir = paths.dirname(suite)
    suite_name = paths.basename(suite)
    suite_path = paths.join(suite_dir, dir_name)
    if is_target(suite):
        files = []
    else:
        files = glob([paths.join(suite_path, "**")])
    if prefix != None:
        target_suffix = "{}_{}".format(prefix, suite_name)
    else:
        target_suffix = suite_name
    if len(files):
        # generate target for data dir
        file_mapping(
            name = "{}-{}".format(dir_name, target_suffix),
            mapping = preserve_structure(
                path = suite_path,
            ),
        )
        return ":{}-{}".format(dir_name, suite_name)
    return ""

def is_target(suite: str) -> bool:
    if suite.startswith(":"):
        return True
    if suite.find("//") != -1:
        return True
    return False

def get_re_executor_from_props(ctx: AnalysisContext) -> [CommandExecutorConfig, None]:
    """
    Convert the `remote_execution` properties param into a `CommandExecutorConfig`
    to use with test providers.
    """

    re_props = ctx.attrs.remote_execution
    if re_props == None:
        return None

    re_props_copy = dict(re_props)
    capabilities = re_props_copy.pop("capabilities")
    use_case = re_props_copy.pop("use_case")
    remote_cache_enabled = re_props_copy.pop("remote_cache_enabled", None)
    if re_props_copy:
        unexpected_props = ", ".join(re_props_copy.keys())
        fail("found unexpected re props: " + unexpected_props)

    return CommandExecutorConfig(
        local_enabled = False,
        remote_enabled = True,
        remote_execution_properties = capabilities,
        remote_execution_use_case = use_case or "tpx-default",
        remote_cache_enabled = remote_cache_enabled,
        remote_execution_action_key = None,
    )
