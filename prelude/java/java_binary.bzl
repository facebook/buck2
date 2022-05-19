load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/linking:shared_libraries.bzl", "SharedLibraryInfo", "merge_shared_libraries", "traverse_shared_library_info")
load("@fbcode//buck2/prelude/utils:utils.bzl", "filter_and_map_idx")
load(
    ":java_providers.bzl",
    "create_template_info",
    "derive_compiling_deps",
    "get_all_java_packaging_deps",
)

def _create_fat_jar(
        ctx: "context",
        java_toolchain: JavaToolchainInfo.type,
        jars: ["artifact"],
        native_libs: ["artifact"]) -> "artifact":
    fat_jar_out = ctx.actions.declare_output("{}.jar".format(ctx.label.name))

    args = [
        java_toolchain.fat_jar[RunInfo],
        "--jar_tool",
        java_toolchain.jar,
        "--output",
        fat_jar_out.as_output(),
        "--jars_file",
        ctx.actions.write("jars_to_merge", jars),
    ]

    if native_libs:
        args += [
            "--fat_jar_lib",
            java_toolchain.fat_jar_main_class_lib,
            "--native_libs_file",
            ctx.actions.write("native_libs", native_libs),
            # fat jar's main class
            "--fat_jar_main_class",
            "com.facebook.buck.jvm.java.FatJarMain",
            # native libraries directory name. Main class expects to find libraries packed inside this directory.
            "--fat_jar_native_libs_directory_name",
            "nativelibs",
        ]

    main_class = ctx.attr.main_class
    if main_class:
        args += ["--main_class", main_class]

    manifest_file = ctx.attr.manifest_file
    if manifest_file:
        args += ["--manifest", manifest_file]

    blocklist = ctx.attr.blacklist
    if blocklist:
        args += ["--blocklist", ctx.actions.write("blocklist_args", blocklist)]

    if ctx.attr.meta_inf_directory:
        args += ["--meta_inf_directory", ctx.attr.meta_inf_directory]

    fat_jar_cmd = cmd_args(args)
    fat_jar_cmd.hidden(jars, native_libs)

    ctx.actions.run(fat_jar_cmd, category = "fat_jar")
    return fat_jar_out

def java_binary_impl(ctx: "context") -> ["provider"]:
    """
     java_binary() rule implementation

    Args:
        ctx: rule analysis context
    Returns:
        list of created providers (DefaultInfo and RunInfo)
    """

    packaging_deps = [packaging_dep.jar for packaging_dep in get_all_java_packaging_deps(ctx, ctx.attr.deps)]

    first_order_deps = derive_compiling_deps(ctx.actions, None, ctx.attr.deps)
    first_order_libs = [dep.full_library for dep in (list(first_order_deps.traverse()) if first_order_deps else [])]

    shared_library_info = merge_shared_libraries(
        ctx.actions,
        deps = filter_and_map_idx(SharedLibraryInfo, ctx.attr.deps),
    )
    native_deps = [shared_lib.lib.output for shared_lib in traverse_shared_library_info(shared_library_info).values()]

    java_toolchain = ctx.attr._java_toolchain[JavaToolchainInfo]
    fat_jar_output = _create_fat_jar(ctx, java_toolchain, packaging_deps, native_deps)

    java_cmd = cmd_args(java_toolchain.java)
    java_cmd.add("-jar", fat_jar_output)

    return [
        DefaultInfo(default_outputs = [fat_jar_output]),
        RunInfo(args = java_cmd),
        create_template_info(packaging_deps, first_order_libs),
    ]
