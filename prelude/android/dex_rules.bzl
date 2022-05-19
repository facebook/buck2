load("@fbcode//buck2/prelude/android:android_providers.bzl", "DexFilesInfo")
load("@fbcode//buck2/prelude/android:class_name_filter.bzl", "class_name_matches_filter", "get_class_name_filter")
load("@fbcode//buck2/prelude/java:dex.bzl", "get_dex_produced_from_java_library")
load("@fbcode//buck2/prelude/java:dex_toolchain.bzl", "DexToolchainInfo")
load("@fbcode//buck2/prelude/java:java_library.bzl", "compile_to_jar")
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect", "flatten")

_DEX_MERGE_OPTIONS = ["--no-desugar", "--no-optimize"]

SplitDexMergeConfig = record(
    dex_compression = str.type,
    primary_dex_patterns = [str.type],
    secondary_dex_weight_limit_bytes = int.type,
)

def _get_dex_compression(ctx: "context") -> str.type:
    is_exopackage_enabled_for_secondary_dexes = "secondary_dex" in ctx.attr.exopackage_modes
    default_dex_compression = "jar" if is_exopackage_enabled_for_secondary_dexes else "raw"
    dex_compression = ctx.attr.dex_compression or default_dex_compression
    expect(
        dex_compression in ["raw", "jar", "xz", "xzs"],
        "Only 'raw', 'jar', 'xz' and 'xzs' dex compression are supported at this time!",
    )

    return dex_compression

def get_split_dex_merge_config(
        ctx: "context",
        android_toolchain: "AndroidToolchainInfo") -> "SplitDexMergeConfig":
    return SplitDexMergeConfig(
        dex_compression = _get_dex_compression(ctx),
        primary_dex_patterns = ctx.attr.primary_dex_patterns,
        secondary_dex_weight_limit_bytes = (
            ctx.attr.secondary_dex_weight_limit or
            android_toolchain.secondary_dex_weight_limit
        ),
    )

def get_single_primary_dex(
        ctx: "context",
        android_toolchain: "AndroidToolchainInfo",
        java_library_jars: ["artifact"],
        is_optimized: bool.type) -> "DexFilesInfo":
    d8_cmd = cmd_args(android_toolchain.d8_command[RunInfo])

    output_dex_file = ctx.actions.declare_output("classes.dex")
    d8_cmd.add(["--output-dex-file", output_dex_file.as_output()])

    jar_to_dex_file = ctx.actions.write("jar_to_dex_file.txt", java_library_jars)
    d8_cmd.add(["--files-to-dex-list", jar_to_dex_file])
    d8_cmd.hidden(java_library_jars)

    d8_cmd.add(["--android-jar", android_toolchain.android_jar])
    if not is_optimized:
        d8_cmd.add("--no-optimize")

    ctx.actions.run(d8_cmd, category = "d8", identifier = "{}:{}".format(ctx.label.package, ctx.label.name))

    return DexFilesInfo(
        primary_dex = output_dex_file,
        secondary_dex_dirs = [],
        proguard_text_files_path = None,
    )

def get_multi_dex(
        ctx: "context",
        android_toolchain: "AndroidToolchainInfo",
        java_library_jars: ["artifact"],
        primary_dex_patterns: [str.type],
        proguard_configuration_output_file: ["artifact", None],
        proguard_mapping_output_file: ["artifact", None],
        is_optimized: bool.type) -> "DexFilesInfo":
    multi_dex_cmd = cmd_args(android_toolchain.multi_dex_command[RunInfo])

    output_dex_file = ctx.actions.declare_output("classes.dex")
    multi_dex_cmd.add("--primary-dex", output_dex_file.as_output())
    secondary_dex_dir = ctx.actions.declare_output("secondary_dex_output_dir")
    multi_dex_cmd.add("--secondary-dex-output-dir", secondary_dex_dir.as_output())

    multi_dex_cmd.add("--primary-dex-patterns-path", ctx.actions.write("primary_dex_patterns", primary_dex_patterns))

    jar_to_dex_file = ctx.actions.write("jars_to_dex_file.txt", java_library_jars)
    multi_dex_cmd.add("--files-to-dex-list", jar_to_dex_file)
    multi_dex_cmd.hidden(java_library_jars)

    multi_dex_cmd.add("--android-jar", android_toolchain.android_jar)
    if not is_optimized:
        multi_dex_cmd.add("--no-optimize")

    if proguard_configuration_output_file:
        multi_dex_cmd.add("--proguard-configuration-file", proguard_configuration_output_file)
        multi_dex_cmd.add("--proguard-mapping-file", proguard_mapping_output_file)

    multi_dex_cmd.add("--compression", _get_dex_compression(ctx))
    multi_dex_cmd.add("--xz-compression-level", str(ctx.attr.xz_compression_level))
    if ctx.attr.minimize_primary_dex_size:
        multi_dex_cmd.add("--minimize-primary-dex")

    ctx.actions.run(multi_dex_cmd, category = "multi_dex", identifier = "{}:{}".format(ctx.label.package, ctx.label.name))

    return DexFilesInfo(
        primary_dex = output_dex_file,
        secondary_dex_dirs = secondary_dex_dir,
        proguard_text_files_path = None,
    )

def merge_to_single_dex(
        ctx: "context",
        android_toolchain: "AndroidToolchainInfo",
        pre_dexed_libs: ["DexLibraryInfo"]) -> "DexFilesInfo":
    output_dex_file = ctx.actions.declare_output("classes.dex")
    pre_dexed_artifacts_to_dex_file = ctx.actions.declare_output("pre_dexed_artifacts_to_dex_file.txt")
    pre_dexed_artifacts = [pre_dexed_lib.dex for pre_dexed_lib in pre_dexed_libs if pre_dexed_lib.dex != None]
    _merge_dexes(ctx, android_toolchain, output_dex_file, pre_dexed_artifacts, pre_dexed_artifacts_to_dex_file)

    return DexFilesInfo(
        primary_dex = output_dex_file,
        secondary_dex_dirs = [],
        proguard_text_files_path = None,
    )

PrimaryDexInput = record(
    lib = "DexLibraryInfo",
    primary_dex_class_names = [str.type],
    has_secondary_dex_class_names = bool.type,
)

def merge_to_split_dex(
        ctx: "context",
        android_toolchain: "AndroidToolchainInfo",
        pre_dexed_libs: ["DexLibraryInfo"],
        split_dex_merge_config: "SplitDexMergeConfig") -> "DexFilesInfo":
    input_artifacts = flatten([[pre_dexed_lib.dex, pre_dexed_lib.class_names, pre_dexed_lib.weight_estimate] for pre_dexed_lib in pre_dexed_libs])
    primary_dex_artifact_list = ctx.actions.declare_output("pre_dexed_artifacts_for_primary_dex.txt")
    primary_dex_output = ctx.actions.declare_output("classes.dex")
    raw_secondary_dexes_dir = ctx.actions.declare_output("raw_secondary_dexes")

    outputs = [primary_dex_output, primary_dex_artifact_list, raw_secondary_dexes_dir]

    def merge_pre_dexed_libs(ctx: "context"):
        primary_dex_inputs, secondary_dex_inputs = _sort_pre_dexed_files(ctx, pre_dexed_libs, split_dex_merge_config)
        secondary_dexes_for_symlinking = {}

        pre_dexed_artifacts = [primary_dex_input.lib.dex for primary_dex_input in primary_dex_inputs if primary_dex_input.lib.dex]
        primary_dex_class_list = ctx.actions.write(
            "class_list_for_primary_dex.txt",
            flatten([primary_dex_input.primary_dex_class_names for primary_dex_input in primary_dex_inputs]),
        )
        has_secondary_dex_classes = any(filter(lambda input: input.has_secondary_dex_class_names, primary_dex_inputs))
        secondary_dex_output = None
        if has_secondary_dex_classes:
            index = len(secondary_dex_inputs)
            secondary_dex_path = _get_raw_secondary_dex_path(index)
            secondary_dex_output = ctx.actions.declare_output(secondary_dex_path)
            secondary_dexes_for_symlinking[secondary_dex_path] = secondary_dex_output
            canary_class_dex_info = _create_canary_class(
                ctx,
                len(secondary_dex_inputs) + 1,
                ctx.attr._dex_toolchain[DexToolchainInfo],
                ctx.attr._java_toolchain[JavaToolchainInfo],
            )
            pre_dexed_artifacts.append(canary_class_dex_info.dex)
        _merge_dexes(
            ctx,
            android_toolchain,
            ctx.outputs[primary_dex_output],
            pre_dexed_artifacts,
            ctx.outputs[primary_dex_artifact_list],
            primary_dex_class_list,
            secondary_dex_output,
        )

        for i in range(len(secondary_dex_inputs)):
            secondary_dex_path = _get_raw_secondary_dex_path(i)
            secondary_dex_output = ctx.actions.declare_output(secondary_dex_path)
            secondary_dex_artifact_list = ctx.actions.declare_output("pre_dexed_artifacts_for_secondary_dex_{}.txt".format(i + 2))
            pre_dexed_artifacts = [secondary_dex_input.dex for secondary_dex_input in secondary_dex_inputs[i] if secondary_dex_input.dex]
            _merge_dexes(ctx, android_toolchain, secondary_dex_output, pre_dexed_artifacts, secondary_dex_artifact_list)

            secondary_dexes_for_symlinking[secondary_dex_path] = secondary_dex_output

        ctx.actions.symlinked_dir(
            output = ctx.outputs[raw_secondary_dexes_dir],
            srcs = secondary_dexes_for_symlinking,
        )

    ctx.actions.dynamic_output(input_artifacts, [], outputs, merge_pre_dexed_libs)

    if split_dex_merge_config.dex_compression == "raw":
        secondary_dex_dir = raw_secondary_dexes_dir
    else:
        secondary_dex_dir = ctx.actions.declare_output("secondary_dexes_dir")

        multi_dex_cmd = cmd_args(android_toolchain.multi_dex_command[RunInfo])
        multi_dex_cmd.add("--secondary-dex-output-dir", secondary_dex_dir.as_output())
        multi_dex_cmd.add("--raw-secondary-dexes-dir", raw_secondary_dexes_dir)
        multi_dex_cmd.add("--compression", _get_dex_compression(ctx))
        multi_dex_cmd.add("--xz-compression-level", str(ctx.attr.xz_compression_level))

        ctx.actions.run(multi_dex_cmd, category = "multi_dex_from_raw_dexes", identifier = "{}:{}".format(ctx.label.package, ctx.label.name))

    return DexFilesInfo(
        primary_dex = primary_dex_output,
        secondary_dex_dirs = [secondary_dex_dir],
        proguard_text_files_path = None,
    )

def _merge_dexes(
        ctx: "context",
        android_toolchain: "AndroidToolchainInfo",
        output_dex_file: "artifact",
        pre_dexed_artifacts: ["artifact"],
        pre_dexed_artifacts_file: "artifact",
        class_names_to_include: ["artifact", None] = None,
        secondary_output_dex_file: ["artifact", None] = None):
    d8_cmd = cmd_args(android_toolchain.d8_command[RunInfo])
    d8_cmd.add(["--output-dex-file", output_dex_file.as_output()])

    pre_dexed_artifacts_to_dex_file = ctx.actions.write(pre_dexed_artifacts_file.as_output(), pre_dexed_artifacts)
    d8_cmd.add(["--files-to-dex-list", pre_dexed_artifacts_to_dex_file])
    d8_cmd.hidden(pre_dexed_artifacts)

    d8_cmd.add(["--android-jar", android_toolchain.android_jar])
    d8_cmd.add(_DEX_MERGE_OPTIONS)

    if class_names_to_include:
        d8_cmd.add(["--primary-dex-class-names-path", class_names_to_include])

    if secondary_output_dex_file:
        d8_cmd.add(["--secondary-output-dex-file", secondary_output_dex_file.as_output()])

    ctx.actions.run(
        d8_cmd,
        category = "d8",
        identifier = "{}:{} {}".format(ctx.label.package, ctx.label.name, output_dex_file.short_path),
    )

def _sort_pre_dexed_files(
        ctx: "context",
        pre_dexed_libs: ["DexLibraryInfo"],
        split_dex_merge_config: "SplitDexMergeConfig") -> (["PrimaryDexInput"], [["DexLibraryInfo"]]):
    primary_dex_class_name_filter = get_class_name_filter(split_dex_merge_config.primary_dex_patterns)
    primary_dex_inputs = []
    secondary_dex_inputs = []
    current_secondary_dex_size = 0
    current_secondary_dex_inputs = []
    for pre_dexed_lib in pre_dexed_libs:
        primary_dex_class_names, has_secondary_dex_class_names = _sort_dex_class_names(
            ctx,
            pre_dexed_lib,
            primary_dex_class_name_filter,
        )

        if len(primary_dex_class_names) > 0:
            primary_dex_inputs.append(
                PrimaryDexInput(
                    lib = pre_dexed_lib,
                    primary_dex_class_names = primary_dex_class_names,
                    has_secondary_dex_class_names = has_secondary_dex_class_names,
                ),
            )
        else:
            weight_estimate = int(ctx.artifacts[pre_dexed_lib.weight_estimate].read_string().strip())
            if current_secondary_dex_size + weight_estimate > split_dex_merge_config.secondary_dex_weight_limit_bytes:
                current_secondary_dex_size = 0
                current_secondary_dex_inputs = []

            if len(current_secondary_dex_inputs) == 0:
                canary_class_dex_info = _create_canary_class(ctx, len(secondary_dex_inputs) + 1, ctx.attr._dex_toolchain[DexToolchainInfo], ctx.attr._java_toolchain[JavaToolchainInfo])
                current_secondary_dex_inputs.append(canary_class_dex_info)
                secondary_dex_inputs.append(current_secondary_dex_inputs)

            current_secondary_dex_size += weight_estimate
            current_secondary_dex_inputs.append(pre_dexed_lib)

    return (primary_dex_inputs, secondary_dex_inputs)

def _sort_dex_class_names(
        ctx: "context",
        pre_dexed_lib: "DexLibraryInfo",
        primary_dex_class_name_filter: "ClassNameFilter") -> ([str.type], bool.type):
    all_java_classes = ctx.artifacts[pre_dexed_lib.class_names].read_string().splitlines()
    primary_dex_class_names = [java_class + ".class" for java_class in all_java_classes if _is_primary_dex_class(java_class, primary_dex_class_name_filter)]
    has_secondary_dex_class_names = len(primary_dex_class_names) < len(all_java_classes)

    return primary_dex_class_names, has_secondary_dex_class_names

def _is_primary_dex_class(class_name: str.type, primary_dex_class_name_filter: "ClassNameFilter"):
    return class_name_matches_filter(class_name, primary_dex_class_name_filter)

def _get_raw_secondary_dex_path(index: int.type):
    return "classes{}.dex".format(index + 2)

# We create "canary" classes and add them to each secondary dex jar to ensure each jar has a class
# that can be safely loaded on any system. This class is used during secondary dex verification.
_CANARY_FILE_NAME_TEMPLATE = "canary_classes/secondary/dex{}/Canary.java"
_CANARY_CLASS_PACKAGE_TEMPLATE = "package secondary.dex{};\n"
_CANARY_CLASS_INTERFACE_DEFINITION = "public interface Canary {}"

def _create_canary_class(ctx: "context", index: int.type, dex_toolchain: DexToolchainInfo.type, java_toolchain: JavaToolchainInfo.type) -> "DexLibraryInfo":
    canary_class_java_file = ctx.actions.write(_CANARY_FILE_NAME_TEMPLATE.format(index), [_CANARY_CLASS_PACKAGE_TEMPLATE.format(index), _CANARY_CLASS_INTERFACE_DEFINITION])
    canary_class_jar = ctx.actions.declare_output("canary_classes/canary_jar_{}.jar".format(index))
    compile_to_jar(ctx, [canary_class_java_file], output = canary_class_jar, javac_tool = java_toolchain.javac, actions_prefix = "canary_class_{}".format(index))

    return get_dex_produced_from_java_library(ctx, dex_toolchain = dex_toolchain, jar_to_dex = canary_class_jar)
