load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/java/utils:java_utils.bzl", "get_path_separator")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

ProguardOutput = record(
    jars = ["artifact"],
    proguard_configuration_output_file = ["artifact", None],
    proguard_mapping_output_file = "artifact",
    proguard_artifacts = ["artifact"],
)

def _get_proguard_command_line_args(
        ctx: "context",
        inputs_to_outputs: {"artifact": "artifact"},
        proguard_configs: ["artifact"],
        mapping: "artifact",
        configuration: ["artifact", None],
        seeds: ["artifact", None],
        usage: ["artifact", None],
        android_toolchain: "AndroidToolchainInfo") -> "cmd_args":
    cmd = cmd_args()
    cmd.add("-basedirectory", "<user.dir>")

    android_sdk_proguard_config = ctx.attr.android_sdk_proguard_config or "none"
    if android_sdk_proguard_config == "optimized":
        cmd.add("-include", android_toolchain.optimized_proguard_config)
        cmd.add("-optimizationpasses", ctx.attr.optimization_passes)
    elif android_sdk_proguard_config == "default":
        cmd.add("-include", android_toolchain.proguard_config)
    else:
        expect(android_sdk_proguard_config == "none")

    for proguard_config in dedupe(proguard_configs):
        cmd.add("-include")
        cmd.add(cmd_args("\"", proguard_config, "\"", delimiter = ""))

    for jar_input, jar_output in inputs_to_outputs.items():
        cmd.add("-injar", jar_input, "-outjar", jar_output if jar_output == jar_input else jar_output.as_output())

    cmd.add("-library")
    cmd.add(cmd_args(android_toolchain.android_bootclasspath, delimiter = get_path_separator()))

    cmd.add("-printmapping", mapping.as_output())
    if configuration:
        cmd.add("-printconfiguration", configuration.as_output())
    if seeds:
        cmd.add("-printseeds", seeds.as_output())
    if usage:
        cmd.add("-printusage", usage.as_output())

    return cmd

def run_proguard(
        ctx: "context",
        android_toolchain: "AndroidToolchainInfo",
        java_toolchain: "JavaToolchainInfo",
        command_line_args_file: "artifact",
        command_line_args: "cmd_args"):
    run_proguard_cmd = cmd_args()
    run_proguard_cmd.add(
        java_toolchain.java[RunInfo],
        "-XX:-MaxFDLimit",
        ctx.attr.proguard_jvm_args,
        "-Xmx{}".format(android_toolchain.proguard_max_heap_size),
        "-jar",
        android_toolchain.proguard_jar,
    )
    run_proguard_cmd.add(cmd_args(command_line_args_file, format = "@{}"))
    run_proguard_cmd.hidden(command_line_args)

    ctx.actions.run(run_proguard_cmd, category = "run_proguard")

# Note that ctx.attr.skip_proguard means that we should create the proguard command line (since
# e.g. Redex might want to consume it) but we don't actually run the proguard command.
def get_proguard_output(
        ctx: "context",
        input_jars: ["artifact"],
        java_packaging_deps: ["JavaPackagingDep"],
        aapt_generated_proguard_config: ["artifact", None]) -> ProguardOutput.type:
    proguard_configs = [packaging_dep.proguard_config for packaging_dep in java_packaging_deps if packaging_dep.proguard_config]
    if ctx.attr.proguard_config:
        proguard_configs.append(ctx.attr.proguard_config)
    if not ctx.attr.ignore_aapt_proguard_config and aapt_generated_proguard_config:
        proguard_configs.append(aapt_generated_proguard_config)

    if ctx.attr.skip_proguard:
        inputs_to_outputs = {input_jar: input_jar for input_jar in input_jars}
        mapping = ctx.actions.write("proguard/mapping.txt", [])
        configuration = None
        seeds = None
        usage = None
    else:
        inputs_to_outputs = {input_jar: ctx.actions.declare_output(
            "proguard_output_jars/{}_{}_obfuscated.jar".format(input_jar.short_path, i),
        ) for i, input_jar in enumerate(input_jars)}
        mapping = ctx.actions.declare_output("proguard/mapping.txt")
        configuration = ctx.actions.declare_output("proguard/configuration.txt")
        seeds = ctx.actions.declare_output("proguard/seeds.txt")
        usage = ctx.actions.declare_output("proguard/usage.txt")

    command_line_args = _get_proguard_command_line_args(
        ctx,
        inputs_to_outputs,
        proguard_configs,
        mapping,
        configuration,
        seeds,
        usage,
        ctx.attr._android_toolchain[AndroidToolchainInfo],
    )

    command_line_args_file = ctx.actions.write("proguard/command-line.txt", command_line_args)

    if ctx.attr.skip_proguard:
        return ProguardOutput(
            jars = input_jars,
            proguard_configuration_output_file = None,
            proguard_mapping_output_file = mapping,
            proguard_artifacts = [command_line_args_file, mapping],
        )
    else:
        output_jars = inputs_to_outputs.values()
        run_proguard(
            ctx,
            ctx.attr._android_toolchain[AndroidToolchainInfo],
            ctx.attr._java_toolchain[JavaToolchainInfo],
            command_line_args_file,
            command_line_args,
        )
        return ProguardOutput(
            jars = output_jars,
            proguard_configuration_output_file = configuration,
            proguard_mapping_output_file = mapping,
            proguard_artifacts = [command_line_args_file, mapping, configuration, seeds, usage],
        )
