load("@fbcode//buck2/prelude/android:android_providers.bzl", "DexFilesInfo")
load("@fbcode//buck2/prelude/java:dex.bzl", "get_dex_produced_from_java_library")
load("@fbcode//buck2/prelude/java:dex_toolchain.bzl", "DexToolchainInfo")
load("@fbcode//buck2/prelude/java:java_library.bzl", "compile_to_jar")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect", "flatten")

_DEX_MERGE_OPTIONS = ["--no-desugar", "--no-optimize"]

SplitDexMergeConfig = record(
    dex_compression = str.type,
    primary_dex_patterns = [str.type],
    secondary_dex_weight_limit_bytes = int.type,
)

def _get_dex_compression(ctx: "context") -> str.type:
    is_exopackage_enabled_for_secondary_dexes = "secondary_dex" in ctx.attrs.exopackage_modes
    default_dex_compression = "jar" if is_exopackage_enabled_for_secondary_dexes else "raw"
    dex_compression = ctx.attrs.dex_compression or default_dex_compression
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
        primary_dex_patterns = ctx.attrs.primary_dex_patterns,
        secondary_dex_weight_limit_bytes = (
            ctx.attrs.secondary_dex_weight_limit or
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
    multi_dex_cmd.add("--xz-compression-level", str(ctx.attrs.xz_compression_level))
    if ctx.attrs.minimize_primary_dex_size:
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

DexInputWithSpecifiedClasses = record(
    lib = "DexLibraryInfo",
    dex_class_names = [str.type],
)

DexInputWithClassNamesFile = record(
    lib = "DexLibraryInfo",
    filtered_class_names_file = "artifact",
)

# When using jar compression, the secondary dex directory consists of N secondary dex jars, each
# of which has a corresponding .meta file (the secondary_dex_metadata_file) containing a single
# line of the form:
# jar:<size of secondary dex jar (in bytes)> dex:<size of uncompressed dex file (in bytes)>
#
# It also contains a metadata.txt file, which consists on N lines, one for each secondary dex
# jar. Those lines consist of:
# <secondary dex jar file name> <hash of secondary dex jar> <canary class>
#
# We write the line that needs to be added to metadata.txt for this secondary dex jar to
# secondary_dex_metadata_line, and we use the secondary_dex_canary_class_name for the
# <canary class>.  When we have finished building all of the secondary dex jars, we read
# each of the secondary_dex_metadata_line artifacts and write them to a single metadata.txt file.
SecondaryDexJarMetadataConfig = record(
    secondary_dex_metadata_path = str.type,
    secondary_dex_metadata_file = "artifact",
    secondary_dex_metadata_line = "artifact",
    secondary_dex_canary_class_name = str.type,
)

def _get_secondary_dex_jar_metadata_config(actions: "actions", secondary_dex_path: str.type, index: int.type) -> SecondaryDexJarMetadataConfig.type:
    secondary_dex_metadata_path = secondary_dex_path + ".meta"
    return SecondaryDexJarMetadataConfig(
        secondary_dex_metadata_path = secondary_dex_metadata_path,
        secondary_dex_metadata_file = actions.declare_output(secondary_dex_metadata_path),
        secondary_dex_metadata_line = actions.declare_output("metadata_line_artifacts/{}".format(index + 1)),
        secondary_dex_canary_class_name = _get_fully_qualified_canary_class_name(index + 1),
    )

def merge_to_split_dex(
        ctx: "context",
        android_toolchain: "AndroidToolchainInfo",
        pre_dexed_libs: ["DexLibraryInfo"],
        split_dex_merge_config: "SplitDexMergeConfig") -> "DexFilesInfo":
    primary_dex_patterns_file = ctx.actions.write("primary_dex_patterns_file", split_dex_merge_config.primary_dex_patterns)

    pre_dexed_lib_with_class_names_files = []
    for pre_dexed_lib in pre_dexed_libs:
        class_names = pre_dexed_lib.class_names
        id = "{}_{}_{}".format(class_names.owner.package, class_names.owner.name, class_names.short_path)
        filtered_class_names_file = ctx.actions.declare_output("primary_dex_class_names_for_{}".format(id))
        filter_dex_cmd = cmd_args([
            android_toolchain.filter_dex_class_names[RunInfo],
            "--primary-dex-patterns",
            primary_dex_patterns_file,
            "--class-names",
            class_names,
            "--output",
            filtered_class_names_file.as_output(),
        ])
        ctx.actions.run(filter_dex_cmd, category = "filter_dex", identifier = id)

        pre_dexed_lib_with_class_names_files.append(
            DexInputWithClassNamesFile(lib = pre_dexed_lib, filtered_class_names_file = filtered_class_names_file),
        )

    input_artifacts = flatten([[input.lib.dex, input.lib.weight_estimate, input.filtered_class_names_file] for input in pre_dexed_lib_with_class_names_files])
    primary_dex_artifact_list = ctx.actions.declare_output("pre_dexed_artifacts_for_primary_dex.txt")
    primary_dex_output = ctx.actions.declare_output("classes.dex")
    initial_secondary_dexes_dir = ctx.actions.declare_output("initial_secondary_dexes_dir")

    outputs = [primary_dex_output, primary_dex_artifact_list, initial_secondary_dexes_dir]

    def merge_pre_dexed_libs(ctx: "context"):
        primary_dex_inputs, secondary_dex_inputs = _sort_pre_dexed_files(ctx, pre_dexed_lib_with_class_names_files, split_dex_merge_config)
        secondary_dexes_for_symlinking = {}

        pre_dexed_artifacts = [primary_dex_input.lib.dex for primary_dex_input in primary_dex_inputs if primary_dex_input.lib.dex]
        primary_dex_class_list = ctx.actions.write(
            "class_list_for_primary_dex.txt",
            flatten([primary_dex_input.dex_class_names for primary_dex_input in primary_dex_inputs]),
        )

        _merge_dexes(
            ctx,
            android_toolchain,
            ctx.outputs[primary_dex_output],
            pre_dexed_artifacts,
            ctx.outputs[primary_dex_artifact_list],
            class_names_to_include = primary_dex_class_list,
        )

        metadata_line_artifacts = []
        for i in range(len(secondary_dex_inputs)):
            if split_dex_merge_config.dex_compression == "jar":
                secondary_dex_path = _get_jar_secondary_dex_path(i)
                secondary_dex_jar_metadata_config = _get_secondary_dex_jar_metadata_config(ctx.actions, secondary_dex_path, i)
                secondary_dexes_for_symlinking[secondary_dex_jar_metadata_config.secondary_dex_metadata_path] = secondary_dex_jar_metadata_config.secondary_dex_metadata_file
                metadata_line_artifacts.append(secondary_dex_jar_metadata_config.secondary_dex_metadata_line)
            else:
                secondary_dex_path = _get_raw_secondary_dex_path(i)
                secondary_dex_jar_metadata_config = None

            secondary_dex_output = ctx.actions.declare_output(secondary_dex_path)
            secondary_dex_artifact_list = ctx.actions.declare_output("pre_dexed_artifacts_for_secondary_dex_{}.txt".format(i + 2))
            secondary_dex_class_list = ctx.actions.write(
                "class_list_for_secondary_dex_{}.txt".format(i + 2),
                flatten([secondary_dex_input.dex_class_names for secondary_dex_input in secondary_dex_inputs[i]]),
            )
            pre_dexed_artifacts = [secondary_dex_input.lib.dex for secondary_dex_input in secondary_dex_inputs[i] if secondary_dex_input.lib.dex]
            _merge_dexes(
                ctx,
                android_toolchain,
                secondary_dex_output,
                pre_dexed_artifacts,
                secondary_dex_artifact_list,
                class_names_to_include = secondary_dex_class_list,
                secondary_dex_jar_metadata_config = secondary_dex_jar_metadata_config,
            )

            secondary_dexes_for_symlinking[secondary_dex_path] = secondary_dex_output

        if split_dex_merge_config.dex_compression == "jar":
            metadata_dot_txt_path = "{}/metadata.txt".format(_SECONDARY_DEX_SUBDIR)
            metadata_dot_txt_file = ctx.actions.declare_output(metadata_dot_txt_path)
            secondary_dexes_for_symlinking[metadata_dot_txt_path] = metadata_dot_txt_file

            def write_metadata_dot_txt(ctx: "context"):
                metadata_lines = []
                for metadata_line_artifact in metadata_line_artifacts:
                    metadata_lines.append(ctx.artifacts[metadata_line_artifact].read_string().strip())
                ctx.actions.write(ctx.outputs[metadata_dot_txt_file], metadata_lines)

            ctx.actions.dynamic_output(metadata_line_artifacts, [], [metadata_dot_txt_file], write_metadata_dot_txt)

        ctx.actions.symlinked_dir(
            ctx.outputs[initial_secondary_dexes_dir],
            secondary_dexes_for_symlinking,
        )

    ctx.actions.dynamic_output(input_artifacts, [], outputs, merge_pre_dexed_libs)

    if split_dex_merge_config.dex_compression == "raw" or split_dex_merge_config.dex_compression == "jar":
        secondary_dex_dir = initial_secondary_dexes_dir
    else:
        secondary_dex_dir = ctx.actions.declare_output("secondary_dexes_dir")

        multi_dex_cmd = cmd_args(android_toolchain.multi_dex_command[RunInfo])
        multi_dex_cmd.add("--secondary-dex-output-dir", secondary_dex_dir.as_output())
        multi_dex_cmd.add("--raw-secondary-dexes-dir", initial_secondary_dexes_dir)
        multi_dex_cmd.add("--compression", _get_dex_compression(ctx))
        multi_dex_cmd.add("--xz-compression-level", str(ctx.attrs.xz_compression_level))

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
        secondary_output_dex_file: ["artifact", None] = None,
        secondary_dex_jar_metadata_config: [SecondaryDexJarMetadataConfig.type, None] = None):
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

    if secondary_dex_jar_metadata_config:
        d8_cmd.add(["--secondary-dex-compression", "jar"])
        d8_cmd.add(["--secondary-dex-metadata-file", secondary_dex_jar_metadata_config.secondary_dex_metadata_file.as_output()])
        d8_cmd.add(["--secondary-dex-metadata-line", secondary_dex_jar_metadata_config.secondary_dex_metadata_line.as_output()])
        d8_cmd.add(["--secondary-dex-canary-class-name", secondary_dex_jar_metadata_config.secondary_dex_canary_class_name])

    ctx.actions.run(
        d8_cmd,
        category = "d8",
        identifier = "{}:{} {}".format(ctx.label.package, ctx.label.name, output_dex_file.short_path),
    )

def _sort_pre_dexed_files(
        ctx: "context",
        pre_dexed_lib_with_class_names_files: ["DexInputWithClassNamesFile"],
        split_dex_merge_config: "SplitDexMergeConfig") -> ([DexInputWithSpecifiedClasses.type], [[DexInputWithSpecifiedClasses.type]]):
    primary_dex_inputs = []
    secondary_dex_inputs = []
    current_secondary_dex_size = 0
    current_secondary_dex_inputs = []
    for pre_dexed_lib_with_class_names_file in pre_dexed_lib_with_class_names_files:
        pre_dexed_lib = pre_dexed_lib_with_class_names_file.lib
        primary_dex_data, secondary_dex_data = ctx.artifacts[pre_dexed_lib_with_class_names_file.filtered_class_names_file].read_string().split(";")
        primary_dex_class_names = primary_dex_data.split(",") if primary_dex_data else []
        secondary_dex_class_names = secondary_dex_data.split(",") if secondary_dex_data else []

        if len(primary_dex_class_names) > 0:
            primary_dex_inputs.append(
                DexInputWithSpecifiedClasses(lib = pre_dexed_lib, dex_class_names = primary_dex_class_names),
            )

        if len(secondary_dex_class_names) > 0:
            weight_estimate = int(ctx.artifacts[pre_dexed_lib.weight_estimate].read_string().strip())
            if current_secondary_dex_size + weight_estimate > split_dex_merge_config.secondary_dex_weight_limit_bytes:
                current_secondary_dex_size = 0
                current_secondary_dex_inputs = []

            if len(current_secondary_dex_inputs) == 0:
                canary_class_dex_input = _create_canary_class(ctx, len(secondary_dex_inputs) + 1, ctx.attrs._dex_toolchain[DexToolchainInfo])
                current_secondary_dex_inputs.append(canary_class_dex_input)
                secondary_dex_inputs.append(current_secondary_dex_inputs)

            current_secondary_dex_size += weight_estimate
            current_secondary_dex_inputs.append(
                DexInputWithSpecifiedClasses(lib = pre_dexed_lib, dex_class_names = secondary_dex_class_names),
            )

    return (primary_dex_inputs, secondary_dex_inputs)

def _get_raw_secondary_dex_path(index: int.type):
    return "classes{}.dex".format(index + 2)

_SECONDARY_DEX_SUBDIR = "assets/secondary-program-dex-jars"

def _get_jar_secondary_dex_path(index: int.type):
    return "{}/secondary-{}.dex.jar".format(_SECONDARY_DEX_SUBDIR, index + 1)

# We create "canary" classes and add them to each secondary dex jar to ensure each jar has a class
# that can be safely loaded on any system. This class is used during secondary dex verification.
_CANARY_FULLY_QUALIFIED_CLASS_NAME_TEMPLATE = "secondary.dex{}.Canary"
_CANARY_FILE_NAME_TEMPLATE = "canary_classes/secondary/dex{}/Canary.java"
_CANARY_CLASS_PACKAGE_TEMPLATE = "package secondary.dex{};\n"
_CANARY_CLASS_INTERFACE_DEFINITION = "public interface Canary {}"

def _create_canary_class(ctx: "context", index: int.type, dex_toolchain: DexToolchainInfo.type) -> DexInputWithSpecifiedClasses.type:
    canary_class_java_file = ctx.actions.write(_CANARY_FILE_NAME_TEMPLATE.format(index), [_CANARY_CLASS_PACKAGE_TEMPLATE.format(index), _CANARY_CLASS_INTERFACE_DEFINITION])
    canary_class_jar = ctx.actions.declare_output("canary_classes/canary_jar_{}.jar".format(index))
    compile_to_jar(ctx, [canary_class_java_file], output = canary_class_jar, actions_prefix = "canary_class{}".format(index))

    dex_library_info = get_dex_produced_from_java_library(ctx, dex_toolchain = dex_toolchain, jar_to_dex = canary_class_jar)

    return DexInputWithSpecifiedClasses(
        lib = dex_library_info,
        dex_class_names = [_get_fully_qualified_canary_class_name(index).replace(".", "/") + ".class"],
    )

def _get_fully_qualified_canary_class_name(index: int.type):
    return _CANARY_FULLY_QUALIFIED_CLASS_NAME_TEMPLATE.format(index)
