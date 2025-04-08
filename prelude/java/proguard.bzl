# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//java:java_providers.bzl",
    "JavaPackagingDep",  # @unused Used as type
)
load("@prelude//java:java_toolchain.bzl", "JavaToolchainInfo")
load("@prelude//java/utils:java_more_utils.bzl", "get_path_separator_for_exec_os")
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//utils:expect.bzl", "expect")

ProguardOutput = record(
    jars_to_owners = dict[Artifact, TargetLabel],
    proguard_configuration_output_file = Artifact | None,
    proguard_mapping_output_file = Artifact,
    proguard_artifacts = list[Artifact],
    proguard_hidden_artifacts = list[Artifact],
)

def _get_proguard_command_line_args(
        ctx: AnalysisContext,
        input_jars_to_output_jars: dict[Artifact, Artifact],
        proguard_configs: list[Artifact],
        additional_jars: list[Artifact],
        mapping: Artifact,
        configuration: Artifact | None,
        seeds: Artifact | None,
        usage: Artifact | None,
        sdk_proguard_config_mode: str | None,
        sdk_proguard_config: Artifact | None,
        sdk_optimized_proguard_config: Artifact | None) -> (cmd_args, list[Artifact]):
    cmd = cmd_args()
    hidden = []
    cmd.add("-basedirectory", "<user.dir>")

    sdk_proguard_config_mode = sdk_proguard_config_mode or "none"
    if sdk_proguard_config_mode == "optimized":
        expect(sdk_optimized_proguard_config)
        cmd.add("-include", sdk_optimized_proguard_config)
        cmd.add("-optimizationpasses", str(ctx.attrs.optimization_passes))
        hidden.append(sdk_optimized_proguard_config)
    elif sdk_proguard_config_mode == "default":
        expect(sdk_proguard_config)
        cmd.add("-include", sdk_proguard_config)
        hidden.append(sdk_proguard_config)
    else:
        expect(sdk_proguard_config_mode == "none")

    for proguard_config in dedupe(proguard_configs):
        cmd.add("-include")
        cmd.add(cmd_args("\"", proguard_config, "\"", delimiter = ""))
        hidden.append(proguard_config)

    for jar_input, jar_output in input_jars_to_output_jars.items():
        cmd.add("-injars", jar_input, "-outjars", jar_output if jar_output == jar_input else jar_output.as_output())

    cmd.add("-libraryjars")
    cmd.add(cmd_args(additional_jars, delimiter = get_path_separator_for_exec_os(ctx)))
    hidden.extend(additional_jars)

    cmd.add("-printmapping", mapping.as_output())
    if configuration:
        cmd.add("-printconfiguration", configuration.as_output())
    if seeds:
        cmd.add("-printseeds", seeds.as_output())
    if usage:
        cmd.add("-printusage", usage.as_output())

    return cmd, hidden

def run_proguard(
        ctx: AnalysisContext,
        java_toolchain: JavaToolchainInfo,
        command_line_args_file: Artifact,
        command_line_args: cmd_args,
        mapping_file: Artifact,
        usage_file: Artifact,
        output_jars: list[Artifact]):
    run_proguard_cmd = cmd_args()
    run_proguard_cmd.add(
        java_toolchain.java[RunInfo],
        "-XX:-MaxFDLimit",
        ctx.attrs.proguard_jvm_args,
        "-Xmx{}".format(java_toolchain.proguard_max_heap_size),
        "-jar",
        java_toolchain.proguard_jar,
    )
    run_proguard_cmd.add(
        cmd_args(command_line_args_file, format = "@{}", hidden = command_line_args),
    )

    output_jars_file = ctx.actions.write("proguard/output_jars.txt", output_jars)

    is_windows = hasattr(ctx.attrs, "_exec_os_type") and ctx.attrs._exec_os_type[OsLookup].platform == "windows"

    # Some proguard configs can propagate the "-dontobfuscate" flag which disables
    # obfuscation and prevents the mapping.txt and usage.txt file from being generated.
    # Scrub all jars emitted from proguard to make them deterministic.
    if not is_windows:
        sh_cmd = cmd_args([
            "sh",
            "-c",
            "touch $1 && touch $2 && $3 && $4 --paths-to-scrub $5 --create-if-not-present",
            "--",
            mapping_file.as_output(),
            usage_file.as_output(),
            cmd_args(run_proguard_cmd, delimiter = " "),
            cmd_args(ctx.attrs._java_toolchain[JavaToolchainInfo].zip_scrubber, delimiter = " "),
            output_jars_file,
        ])
    else:
        sh_cmd = cmd_args([
            "cmd.exe",
            "/c",
            cmd_args([
                cmd_args([mapping_file.as_output()], format = "echo. > {}"),
                cmd_args([usage_file.as_output()], format = "echo. > {}"),
                cmd_args(run_proguard_cmd, delimiter = " "),
                cmd_args(ctx.attrs._java_toolchain[JavaToolchainInfo].zip_scrubber, "--paths-to-scrub", output_jars_file, "--create-if-not-present", delimiter = " "),
            ], delimiter = " && "),
        ])

    ctx.actions.run(sh_cmd, category = "run_proguard")

# Note that ctx.attrs.skip_proguard means that we should create the proguard command line (since
# e.g. Redex might want to consume it) but we don't actually run the proguard command.
def get_proguard_output(
        ctx: AnalysisContext,
        input_jars: dict[Artifact, TargetLabel],
        java_packaging_deps: list[JavaPackagingDep],
        additional_proguard_configs: list[Artifact],
        additional_jars: list[Artifact],
        sdk_proguard_config_mode: str | None,
        sdk_proguard_config: Artifact | None,
        sdk_optimized_proguard_config: Artifact | None,
        skip_proguard: bool = False) -> ProguardOutput:
    proguard_configs = [packaging_dep.proguard_config for packaging_dep in java_packaging_deps if packaging_dep.proguard_config]
    if ctx.attrs.proguard_config:
        proguard_configs.append(ctx.attrs.proguard_config)
    proguard_configs.extend(additional_proguard_configs)

    if skip_proguard:
        input_jars_to_output_jars = {input_jar: input_jar for input_jar in input_jars.keys()}
        mapping = ctx.actions.write("proguard/mapping.txt", [])
        configuration = None
        seeds = None
        usage = None
    else:
        input_jars_to_output_jars = {input_jar: ctx.actions.declare_output(
            "proguard_output_jars/{}_{}_obfuscated.jar".format(input_jar.short_path, i),
        ) for i, input_jar in enumerate(input_jars.keys())}
        mapping = ctx.actions.declare_output("proguard/mapping.txt")
        configuration = ctx.actions.declare_output("proguard/configuration.txt")
        seeds = ctx.actions.declare_output("proguard/seeds.txt")
        usage = ctx.actions.declare_output("proguard/usage.txt")

    command_line_args, hidden_artifacts = _get_proguard_command_line_args(
        ctx,
        input_jars_to_output_jars,
        proguard_configs,
        additional_jars,
        mapping,
        configuration,
        seeds,
        usage,
        sdk_proguard_config_mode,
        sdk_proguard_config,
        sdk_optimized_proguard_config,
    )

    command_line_args_file = ctx.actions.write("proguard/command-line.txt", command_line_args)

    if skip_proguard:
        return ProguardOutput(
            jars_to_owners = input_jars,
            proguard_configuration_output_file = None,
            proguard_mapping_output_file = mapping,
            proguard_artifacts = [command_line_args_file, mapping],
            proguard_hidden_artifacts = hidden_artifacts,
        )
    else:
        run_proguard(
            ctx,
            ctx.attrs._java_toolchain[JavaToolchainInfo],
            command_line_args_file,
            command_line_args,
            mapping,
            usage,
            input_jars_to_output_jars.values(),
        )
        output_jars = {output: input_jars[input_jar] for input_jar, output in input_jars_to_output_jars.items()}
        return ProguardOutput(
            jars_to_owners = output_jars,
            proguard_configuration_output_file = configuration,
            proguard_mapping_output_file = mapping,
            proguard_artifacts = [command_line_args_file, mapping, configuration, seeds, usage],
            proguard_hidden_artifacts = hidden_artifacts,
        )
